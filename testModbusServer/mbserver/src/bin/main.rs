use std::env::args;
use std::fs::File;
use std::io::prelude::*;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::Duration;
use tokio_modbus::prelude::*;

use mbserver::*;

fn main() {
    // Since the server will only be used within a testing enviroment, simply panic on each fault
    let filename = args().nth(1).expect("mbserver: no register file given");

    // read thje template file
    let mut file = File::open(filename).expect("mbserver: error opening register file");
    let mut contents = String::new();
    file.read_to_string(&mut contents)
        .expect("mbserver: error reading register file");
    std::mem::drop(file);

    // parse registers and create the server
    let registers = parse_registers(&contents);
    let mbserver = create_server(registers);

    // the test server is always running on localhost:5502 to avoid the need to be
    // run as super user
    let socket_addr = "127.0.0.1:5502".parse().unwrap();

    let server = Arc::new(Mutex::new(mbserver));

    // spawn the serever in a dedicated thread
    thread::spawn(move || {
        server::tcp::Server::new(socket_addr)
            .serve(move || Ok(Arc::clone(&server).lock().unwrap().clone()));
    });
    // Give the server some time for stating up
    thread::sleep(Duration::from_secs(1));

    loop {
        // give the thread something to do :)
        thread::sleep(Duration::from_millis(30));
    }
}
