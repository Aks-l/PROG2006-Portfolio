// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.26;

contract RPC{

    struct Game{
        string id;
        bytes32 p1;
        bytes32 p2;
        string v1;
        string v2;
        bytes32 winner;
        int stake;
    }

    event joinGame(address sender, bytes32 vote);
    event revealVote(address sender, string vote);
    event withdrawGame(address sender, int amount);

    int minStake = 2;

    string[3] votes = ["R", "P", "S"];

    function play(string memory vote) public {
        
    }
    
    function withdraw() public {

    }

    function reveal() public {

    }


}