// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.26;

contract RPC{

    struct Game{
        string id;
        address p1;
        address p2;
        bytes32 v1;
        bytes32 v2;
        address winner;
        int stake;
    }

    // maps player to game
    mapping(address => Game) public activeGames;

    // map game id to owner (game.p1)
    mapping(string => address) public gameOwners;

    event joinGame(address sender, bytes32 vote);
    event revealVote(address sender, string vote);
    event withdrawGame(address sender, int amount);

    int minStake = 2;

    string[3] votes = ["R", "P", "S"];

    function play(bytes32 _vote, int _stake, string memory _id) public payable {
        // if any game has the same stake, join that game
        if (_stake < minStake)
            return;

        // if game does not exist
        if (activeGames[msg.sender].p1 != address(0))
            return;
        
        // if game has no owner, create new game
        if (gameOwners[_id] == address(0)){

            activeGames[msg.sender] = Game({
                id: _id,
                p1: msg.sender,
                p2: address(0),
                v1: _vote,
                v2: bytes32(0),
                winner: address(0),
                stake: _stake
            });

            gameOwners[_id] == msg.sender;
        }
        // otherwise join game
        // find game through p1 map
        else {
            Game storage toJoin = activeGames[gameOwners[_id]];
            if (toJoin.stake != _stake)
                return;

            toJoin.p2 = msg.sender;
            toJoin.v2 = _vote;

        }
    }
    
    function withdraw() public {

    }

    function reveal() public {
    

    }


}