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
        uint startTime;
    }

    mapping(string => Game) public activeGames;
    mapping(address => bool) public isInGame;

    event joinGame(address sender, bytes32 vote);
    event revealVote(address sender, string vote);
    event withdrawGame(address sender, int amount);
    event draw(string message);

    uint minStake = 2;


    function play(bytes32 _vote,string memory _id) public payable {
        require(!(isInGame[msg.sender]), "You are already in a game");
        require(msg.value >= minStake, "Stake too low");

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
                stake: msg.value,
                startTime: block.timestamp
            });
        } 

        // if there is one player in game
        else {
            Game storage game = activeGames[_id];
            require(game.stake == msg.value, "Wrong stake");
            require(game.p2 == address(0), "Game full");

            game.p2 = msg.sender;
            game.v2 = _vote;
            game.startTime = block.timestamp;
        }

        isInGame[msg.sender] = true;
        emit joinGame(msg.sender, _vote);
    }
    
    function reveal(string memory vote, string memory salt, string memory _id) public {
        bytes32 hashedVote = keccak256(abi.encodePacked(vote, salt));

        require(activeGames[_id].p2 != address(0), "Player 2 not joined yet.");

        require(msg.sender == activeGames[_id].p1 || msg.sender == activeGames[_id].p2, "You are not a part of this game.");

        require(hashedVote != activeGames[_id].v1 || hashedVote != activeGames[_id].v2, "Wrong vote/salt.");

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
            if   (winner == 0){
                payable(activeGames[_id].p1).transfer(activeGames[_id].stake);
                payable(activeGames[_id].p2).transfer(activeGames[_id].stake);
                    activeGames[_id] = Game({
                    id: "",
                    p1: address(0),
                    p2: address(0),
                    v1: bytes32(0),
                    v2: bytes32(0),
                    revealed: 99,
                    revealer: address(0),
                    winner: address(0),
                    stake: 0,
                    startTime: 0
                });  
                isInGame[activeGames[_id].p1] = false;
                isInGame[activeGames[_id].p2] = false;
                emit draw("It was a tie, both players payed back their stake.");
                return; 
            }                                                           // draw
            else if (winner == 1)
                activeGames[_id].winner = activeGames[_id].revealer;    // first player to reveal wins
            else if (winner == 2)
                activeGames[_id].winner = msg.sender;                   // second player to reveal wins
        }
        emit revealVote(msg.sender, vote);
    }
        
    function withdraw(string memory _id) public {
        if (activeGames[_id].startTime + 5 minutes < block.timestamp){
            if (activeGames[_id].p2 == address(0))
                payable(msg.sender).transfer(activeGames[_id].stake);
            else
                payable(msg.sender).transfer(activeGames[_id].stake*2);

            activeGames[_id] = Game({
                id: "",
                p1: address(0),
                p2: address(0),
                v1: bytes32(0),
                v2: bytes32(0),
                revealed: 99,
                revealer: address(0),
                winner: address(0),
                stake: 0,
                startTime: 0
            });  
            isInGame[activeGames[_id].p1] = false;
            isInGame[activeGames[_id].p2] = false;
            return;  
        }

        require(activeGames[_id].winner == msg.sender, "You did not win that game (yet)!");
        
        uint payout = activeGames[_id].stake * 2;

        payable(msg.sender).transfer(payout);     

        activeGames[_id] = Game({
                id: "",
                p1: address(0),
                p2: address(0),
                v1: bytes32(0),
                v2: bytes32(0),
                revealed: 99,
                revealer: address(0),
                winner: address(0),
                stake: 0,
                startTime: 0
            });   
        isInGame[activeGames[_id].p1] = false;
        isInGame[activeGames[_id].p2] = false;
    }



    event makeHash(bytes32 hashed, string s);

    function hashThis(string memory str, string memory salt) public {
        bytes32 hashedValue = keccak256(abi.encodePacked(str, salt));
        emit makeHash(hashedValue, "somethinghere");
    }


}