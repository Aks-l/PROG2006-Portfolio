// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.26;

contract HelloWorld{

    event SayHello(string message, string name);

    function hello(string memory _name) public {
        emit SayHello("Hello, ", _name);
    }

}
