# Lab 07: Lottery 🍀

* Deadline (soft): Friday, 21st of March, 16:00
* Deadline (hard): Friday, 4th of April, 16:00 
* Score: (4+1)
* Language: Haskell
* ChatGPT: **use it**. Use it for explanations. You can use it to generate/modify your code. You can use it to explain code to you. 
* Co-pilot: **use is encouraged**. Use it to generate code for you.
* Coding: code with tools and by yourself. **ONLY** use code that you understand.
* **do-notation** is not allowed in this lab.


# Objectives

* Basic structure of more complex programs. Divide and conquer skills. 
* Ability to deal with larger problems and splitting them.
* Problems decomposition into individual parts that need to be combined together.
* You **should use** ChatGPT and co-pilot for code generation, and code understanding.
* Make the tools to generate code that you want and that you understand.
* Make sure you understand all the generated code. Ask ChatGPT or Co-pilot to explain the code to you.
* **Do not use**: do-notation


# The task: Lottery💰

Alright! Let's have some ball lottery fun! Here are the rules:

Step 1: Balls🍀

The lottery machine has balls 🎱 with numbers from 1 to 39. Initially, three balls are used, and three numbers are obtained. These numbers are called the **Winning numbers**. Let us assume for example the winning numbers are: 5, 12 and 35.

Step 2: Go for the Points!
Now, for the second round. This time, 5 balls ale used, and you aim to hit the jackpot with one of the winning numbers. If you succeed, you're in for some points!

- Single digits winning numbers get you 1 point (those are: 1,2,3,4,5,6,7,8,9)
- Numbers between `10` and `19` snag you `2` points.
- Numbers between `20` and `29` give you `4` points.
- Big shots from `30` to `39` rack up `8` points!

But here's the twist! If you have multiple winning numbers, you're in for a double treat. For instance, if you nail a ball with 12, that's 2 points (thanks to being in the 10-19 range). If there is a ball with another 12 in your five balls round, it is now double, and it gives 4 points. So for two 12s you would get 6 points in total! Similarly, if you got two 35s, the first ball gives 8, the second 16, and in total you get 24 points.

Step 3: Double Trouble!
There is a TWIST! If you get a winning number repeat in the initial 3 balls, your points double up instantly, and if the winning number occurs 3 times in the list of winning numbers, you QUADRUPLE the original scoring! So, if your winning numbers are, say, 12, 12, and 24, and in your 5 ball round you got one ball with 12, you're cashing in 4 points, not just 2! And if it happens that the winning numbers are 12, 12, 12 (three times 12), your first ball with 12 will give you immediately 8 points! And if you happen to have second ball with 12, thats 16 points extra, which means two balls with 12 with the 3x 12 as winning numbers will give you 24 points.
If the winning numbers are in the Big Shots range, say you got 35s three times as the winning numbers, then the first 35 in your five ball round gives you a whooping 32 (`8*2*2`) points!

Step 4: Tally Up the Fun!
All the balls are represented in a cool sequence of integer numbers. For instance, 8 14 16, representing the "winning numbers" followed by the scoring throws of: 5 8 18 16 12. Check it out and see how many points you've scored. 
For the sequence: `8 14 16 5 8 18 16 12` we have:
* one scoring `8` matching a winning `8` - 1 point (single digit)
* one scoring `16` matching a winning `16` - 2 points (numbers between 10-19)
* Total: `3` points
For the sequence `8 14 16 5 8 14 16 14`, you have a whopping `9` points! That's `2` for the first `14`, `4` for the second `14`, and an extra `2` for the `16`, plus `1` for the `8`! Total `9` points.

Now, you're on a mission to process all the lottery game data. Each row has the first 3 numbers as the winning numbers and the next 5 as the scoring numbers. Your task is to score each row, add 'em up all together. 



# Instructions

* Make your code modular. Split the functionality into individual functions
that are composed together to form the final result
* Keep all your functions pure, and only use `getContents` in the `main` function, as declared in the skeleton code
* Use `doctests` to test your functions
* Make sure that ALL your implemented functions have at least few test cases as `doctests`
* Make sure that `stack test` executes all your `doctests` and they are all OK
* Test data as comments and real data are provided in the repo: `data.txt` 
* The total for test data is: **?** 
   * The first student to post (Issue Tracker comment to the Lab 07) the total for `data.txt` will get +1 kudos!



# Gitlab and git

The `lab07` project is pre-initiated for the task. Fork the project into your own workspace
and populate the missing parts.  Make sure that the test harness passes correctly. 



# IMPORTANT

* Do not use `do-notation`. Use the manual bind operator `(>>=)`
* Generated code from co-pilot is **encouraged**. Use co-pilot and ChatGPT. Only use code you understand. Do not use operators that you do not understand.
* Try to compose your functions with the use applicatives and `<$>` and `<*>` operators. 


# Bonus point (+1)

* Observe, that the rewards always form a sequence: `1, 2, 4, 8, 16, ...`
* You get +1 if you can exploit this to make your code more modular and re-usable.