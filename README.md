# Modbus Client

# Table of Contents

1. [Introduction](#introduction)
2. [Registers Table](#register-table)
3. [Word Order](#word-order)
4. [Building Modbus Client](#building-modbus-client)

## Introduction

*Modbus Client* is an interactive Modbus communication tool. *Modbus Client* can be used as:

- A command line interactive repl. The repl can be started using the ``-r`` command line flag.

    The following command will start a repl connected to a Modbus TCP server listening to IP address 192.168.1.2 and Port 502

    ````bash
    > modbus-client -r --TCP --ip 192.168.2.1 --port 502
    ````

    A valid connection to a Modbus server is necessary to start the *Modbus Client* repl

- An HTML GUI that can be accessed locally at ``http://localhost:4000``.

    The following command will start a local web server and try to open the default browser at ``http://localhost:4000``

    ````bash
    > modbus-client -s
    ````

    The *Modbus Client* GUI can be started without an already established connection to a Modbus Server. If the server is not able to automatically staret your web browser, please visit ``http://localhost:4000`` manually.

- A command line tool to read Modbus registers from a template file

    The following command will connect to a Modbus TCP server listening to IP address 192.168.1.2 and Port 502 and query all registers found on ``input.csv`` file outputing the result to ``output.csv``

    ````bash
    > modbus-client -t --TCP --ip 192.168.2.1 --port 502 --i input.csv -o output.csv
    ````

    A valid connection to a Modbus server is necessary to start the *Modbusclient* repl

For more information on availiable commands and options, use the ``-h, --help`` flags

````bash
> modbus-client --help
````

## Registers Table

*Modbus Client* uses a registers table format to make working with multiple registers easier.

The registers table is a colon seperated csv file. You can find a sample table in the in the installation folder called ``sample.csv``.

The following raw csv file

``` csv
Name;Register Type;Register Address;Data Type;Data Value;Unit Id;Description
status;input register;10;Word;1;1;0 = device is stopped, 1 = device is running
command;holding register;10;Word;0;1;0 = stop device, 1 = start device
setpoint;holding register;15;Float;;1;power setpoint
temperature;input register;20;Double;35.2;1;device temperature
alarms;input register;22;Bits;0001100100110101;1;alarms
```

corresponds to the register table below:

| Name          | Register Type     | Register Address | Data Type | Data Value | Unit Id | Description |
| :----         | :-------------    | :----------------| :---------| :----------| :-------| :---------- |
| status        | Input Register    | 10 | Word   |      | 1 | 0 = device is stopped, 1 = device is running |
| command       | Holding Register  | 10 | Word   | 1    | 1 | 0 = stop device, 1 = start device |
| setpoint      | Holding Register  | 15 | Float  | 3.14 | 1 | power setpoint |
| temperature   | Input Register    | 20 | Double |      | 1 | device temperature |
| alarms        | Input Register    | 22 | Bits   |      | 1 | device alarms |

The first line of a registers table csv file is always ignored and is only used as column header information. Each subsequent row is parsed by *Modbus Client* using the parsing rules described below:

#### Name

The name field serves as an identifier for the register. The following naming rules apply:

- A name field cannot start with a numeric value
- No special characters are allowed, except an underscore "_"
- Only uppercase or lowercase alphabetic ASCII characters are allowed

#### Register Type

Valid register types:

- ``Input Register``
- ``Holding Register``

Capitalization is ignored when parsing the register type field, eg ``INPUT REGISTER`` and ``input register`` are both valid fields.

For more information on Modbus register types see the [Modbus Application Protocol](https://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf)

#### Register Address

Modbus protocol based address for the register. In the case of registers with multiple addresses (eg Float or Double) the address field defines the lowest address.

#### Data Type

- ``Word`` : A 16-bit Word
- ``Float`` : A 32-bit floating point number (2 words)
- ``Double`` : A 64-bit floating point number (4 words)

Capitalization is ignored when parsing the data type field, eg ``Word``, ``word`` and ``WORD`` are valid fields.

For word ordering on types with more than one word represenatation, see [Word Ordering](#word-ordering)

#### Data Value

Initial value for a register. The field can be left empty. Only usefull when defining default values. When reading registers, this value will be overwritten, so keeping the field empty is usually sufficient.

#### Unit Id

Unit Id of the Modbus Server assigned to the register.

#### Description

Description of the register. Cannot contain line breaks or a colon character (;).

## Word Order

Word order defines the order of input words for data values that require multiple words for thei represenatation (eg. Float Word). According to the [Modbus Application Protocol](https://www.modbus.org/docs/Modbus_Application_Protocol_V1_1b.pdf):

>MODBUS  uses  a  ‘big-Endian’  representation  for  addresses  and  data  items.  This  means  that  when  a  numerical  quantity  larger  than  a  single  byte  is  transmitted,  the  most  significant byte is sent first.

Conforming to this requirement, *Modbus Client* always uses a 'big-endian' representaion for the 16-bit word type. In order to allow for some flexibility, for data values requiring more than one word value, a choice between 'little-endian' and 'big-endian' representation can be made.

Example:

Four word values layout out in four input registers as follows:

| Word Address | 0 | 1 | 2 | 3 |
| :--- |:-- | :-- |:-- |:--|
| Word | 0x01 | 0x23 | 0x45 | 0x67  |

will be interpreted as a Double value:

- Little Endian : ``0x67452301``
- Big Endian : ``0x01234567``

## Building Modbus Client

*Modbus-Client* is an open source project. You can download and built the project at the [source repository](https://github.com/3kyro/modbus-client).

### Prerequisites

To build *Modbus Client* on Linux and Windows you will need the following tools installed:

- [The Haskell Tool Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) - version >=2.5.1
- [Elm](https://guide.elm-lang.org/install/elm.html) - version 0.19.1
- [Rust](https://www.rust-lang.org/tools/install) - version >= 1.47.0 . Only for building the Modbus test server
- [NSIS](https://nsis.sourceforge.io/Download). Only for building *Modbus Client* on Windows. You will need to add the NSIS folder to your path after installation

### Building using `build.hs`

You can use the provided `build.hs` script to build *Modbus Client* backend and frontend along with a Modbus test server, as well as Linux and Windows installers. The script can only be run inside the application's root folder.

Linux:

````bash

> ./build.hs

````

Windows:

````bash

> stack runghc .\build.hs

````
