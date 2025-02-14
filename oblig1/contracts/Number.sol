// SPDX-License-Identifier: MIT
pragma solidity 0.8.26;

contract Number {
    uint public number;

    event NumberChanged(uint oldValue, uint newValue);

    function setNumber(uint _num) public {
        uint oldValue = number;
        number = _num;
        emit NumberChanged(oldValue, number);
    }


    function adder(uint a, uint b) public pure returns (uint){

        return a+b;
    }

    function doubleNUmber() public view returns (uint){
        return number * 2;
    }

}
