use futures::future;
use std::io::prelude::*;
use std::io::Result;
use tokio_modbus::prelude::*;
use tokio_modbus::server::*;
use std::sync::{Arc, Mutex};


type Address = u16;
type Coil = bool;
type Word = u16;

pub struct MbServer {
    pub coils: Vec<Coil>,
    pub discrete_inputs: Vec<Coil>,
    pub input_registers: Vec<Word>,
    pub holding_registers: Arc<Mutex<Vec<Word>>>,
}

impl Service for MbServer {
    type Request = Request;
    type Response = Response;
    type Error = std::io::Error;
    type Future = future::Ready<Result<Self::Response>>;

    fn call(&self, req: Self::Request) -> Self::Future {
        dbg!(&req);
        match req {
            Request::ReadInputRegisters(addr, cnt) => {
                let split = &self.input_registers[addr as usize..(addr + cnt) as usize];
                future::ready(Ok(Response::ReadInputRegisters(split.to_owned())))
            }
            Request::ReadHoldingRegisters(addr, cnt) => {
                let split = 
                    self.holding_registers.lock().unwrap().clone().split_off(addr as usize);
                future::ready(Ok(Response::ReadHoldingRegisters(
                    split.iter().take(cnt as usize).copied().collect()
                )))
            }
            Request::WriteSingleRegister(addr, m_word) => {
                self.holding_registers.lock().unwrap().clone()[addr as usize] = m_word;
                let rsp = Response::WriteSingleRegister(
                    addr,
                    self.holding_registers.lock().unwrap()[addr as usize],
                );
                future::ok(rsp)
            }
            Request::WriteMultipleRegisters(addr, regs) => {
                for i in 0..regs.len() {
                    self.holding_registers.lock().unwrap()[addr as usize + i as usize] =
                        regs[i as usize];
                }
                let rsp = Response::WriteMultipleRegisters(addr, regs.len() as u16);
                future::ok(rsp)
            }
            _ => {
                unimplemented!()
            }
        }
    }
}

impl Clone for MbServer {
    fn clone(&self) -> Self {
        MbServer {
            coils: self.coils.clone(),
            discrete_inputs: self.discrete_inputs.clone(),
            input_registers: self.input_registers.clone(),
            holding_registers: self.holding_registers.clone(),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, PartialEq)]
pub enum Register {
    Coil(Address, Coil),
    DiscreteInput(Address, Coil),
    Input(Address, Word),
    Holding(Address, Word),
}

pub fn parse_registers(content: &str) -> Vec<Register> {
    content
        .lines()
        .map(|line| {
            let mut words = line.split_whitespace();
            let reg_type = words.next().expect("mbserver: Error parsing register type");
            let reg_addr = words.next().expect("mbserver: Error parsing register addr");
            let reg_value = words
                .next()
                .expect("mbserver: Error parsing register value");
            match reg_type {
                "input_register" => Register::Input(
                    parse_int::parse(reg_addr).unwrap_or_else(|_| {
                        panic!(
                            "mbserver: Error parsing input register address ({})",
                            reg_addr
                        )
                    }),
                    parse_int::parse(reg_value).unwrap_or_else(|_| {
                        panic!(
                            "mbserver: Error parsing input register value ({})",
                            reg_value
                        )
                    }),
                ),
                "holding_register" => Register::Holding(
                    parse_int::parse(reg_addr).unwrap_or_else(|_| {
                        panic!(
                            "mbserver: Error parsing holding register address ({})",
                            reg_addr
                        )
                    }),
                    parse_int::parse(reg_value).unwrap_or_else(|_| {
                        panic!(
                            "mbserver: Error parsing holding register value ({})",
                            reg_value
                        )
                    }),
                ),
                _ => unimplemented!(),
            }
        })
        .collect()
}

pub fn create_server(registers: Vec<Register>) -> MbServer {
    let mut server = MbServer {
        coils: vec![false; 0xFFFF],
        discrete_inputs: vec![false; 0xFFFF],
        input_registers: vec![0; 0xFFFF],
        holding_registers: Arc::new(Mutex::new(vec![0; 0xFFFF])),
    };

    for reg in registers {
        match reg {
            Register::Input(addr, word) => server.input_registers[addr as usize] = word,
            Register::Holding(addr, word) => server.holding_registers.lock().unwrap().clone()[addr as usize] = word,
            _ => unimplemented!(),
        }
    }

    server
}

mod tests {
    use super::*;

    #[allow(dead_code)]
    fn tmp_registe_file() -> Vec<Register> {
        use std::io::SeekFrom;
        use tempfile::*;
        let mut tmpfile = tempfile().unwrap();

        let content = b"input_register 10 1500\nholding_register 15 1700\ninput_register 11 0xDEAD\nholding_register 16 0xBEEF";
        tmpfile.write_all(content).unwrap();
        tmpfile.seek(SeekFrom::Start(0)).unwrap();

        let mut content = String::new();
        tmpfile.read_to_string(&mut content).unwrap();

        parse_registers(&content)
    }

    #[test]
    fn read_and_parse_registers() {
        let registers = tmp_registe_file();

        assert_eq!(registers[0], Register::Input(10, 1500));
        assert_eq!(registers[1], Register::Holding(15, 1700));
        assert_eq!(registers[2], Register::Input(11, 0xDEAD));
        assert_eq!(registers[3], Register::Holding(16, 0xBEEF));
    }

    #[test]
    fn respond_to_modbus_requests() {
        use std::thread;

        let socket_addr = "127.0.0.1:5502".parse().unwrap();

        let registers = tmp_registe_file();
        let server = create_server(registers);

        thread::spawn(move || {
            server::tcp::Server::new(socket_addr).serve(move || Ok(server.clone()));
        });

        let mut ctx = sync::tcp::connect(socket_addr).unwrap();
        let input = ctx.read_input_registers(10, 2).unwrap();
        let holding = ctx.read_holding_registers(15, 2).unwrap();

        assert_eq!(input[0], 1500);
        assert_eq!(input[1], 0xDEAD);
        assert_eq!(holding[0], 1700);
        assert_eq!(holding[1], 0xBEEF);
    }
}
