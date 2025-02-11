// SPDX-License-Identifier: GPL-3.0
pragma solidity ^0.8.26;

contract RPC{

    struct Game{
        string id;
        address p1;
        address p2;
        bytes32 v1;
        bytes32 v2;
        uint revealed;
        address revealer;
        address winner;
        uint stake;
    }

    mapping(string => Game) public activeGames;
    mapping(address => bool) public isInGame;

    event joinGame(address sender, bytes32 vote);
    event revealVote(address sender, string vote);
    event withdrawGame(address sender, int amount);

    uint minStake = 2;

    enum votes {R, P, S}

    function play(bytes32 _vote, uint _stake, string memory _id) public payable {
        if (isInGame[msg.sender])
            return;
        if (_stake < minStake)
            return;

        // if there is no player in game
        if (activeGames[_id].p1 == address(0)) {
            activeGames[_id] = Game({
                id: _id,
                p1: msg.sender,
                p2: address(0),
                v1: _vote,
                v2: bytes32(0),
                revealed: 99,
                revealer: address(0),
                winner: address(0),
                stake: _stake
            });
        } 

        // if there is one player in game
        else {
            Game storage game = activeGames[_id];
            require(game.stake == _stake, "Wrong stake");
            require(game.p2 != address(0), "Game full");

            game.p2 = msg.sender;
            game.v2 = _vote;
        }

        isInGame[msg.sender] = true;
        emit joinGame(msg.sender, _vote);
    }
    


    function reveal(string memory vote, string memory salt, string memory _id) public {
        bytes32 hashedVote = keccak256(abi.encodePacked(vote, salt));
        bytes32 playedVote = activeGames[_id].v1;
       
        if (msg.sender != activeGames[_id].p1 && msg.sender != activeGames[_id].p2)
            return; // not in game

        if (hashedVote != playedVote)
            return; // wrong vote/salt

        uint voteScore;
        if (bytes(vote)[0] == "R")
            voteScore = 0;
        else if (bytes(vote)[0] == "P")
            voteScore = 1;
        else if (bytes(vote)[0] == "S")
            voteScore = 2;

        if (activeGames[_id].revealer == address(0)){
            activeGames[_id].revealed = voteScore;
            activeGames[_id].revealer = msg.sender;
        } else {
            uint winner = (activeGames[_id].revealed - voteScore) % 3;
            if      (winner == 0)
                activeGames[_id].winner = address(1); // draw
            else if (winner == 1)
                activeGames[_id].winner = activeGames[_id].revealer; // first player to reveal wins
            else if (winner == 2)
                activeGames[_id].winner = msg.sender; // second player to reveal wins
        }
        emit revealVote(msg.sender, vote);
    }
        

    function withdraw() public {
  
    }

}