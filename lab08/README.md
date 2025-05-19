# Lab 08: Students and Birds

* Deadline (soft): Friday, 28th of March, 16:00
* Deadline (hard): Friday, 4th of April, 16:00
* Score: 4
* Language: Rust
* ChatGPT: **do not use**. Write all the code yourself.
* Co-pilot: **do not use**.
* Coding: code only by yourself. Use Rust book and Rust documentation. **Do not use** any external crates.



# Important

* Pay attention to the Rust coding style. 
* Make the code easy to maintain, readable, and concise.
* Pay attention to the code performance. Use "single pass over data" if that is possible.



# Reverse a string

* `reverse`
   * Implement the function `reverse` to return the reversed input string.
   * **DO NOT USE** `rev()` built-in function for this task.
* There is no point doing it with ChatGPT - do it yourself.


# Oldest students count

Given an input data in specific format, find which students are the oldest, and count them.
The data format follows simple rules:
* the first item in every line is the name of the student
* the second item, separated by whitespace is the student surname
* the third item, again separated by whitespace, is the student age, as integer value
The example data string is hardcoded in the program.

For the following data, the answer should be 3.
The oldest people are Cooper, Chains and Chaplin,
thus the count of oldest students is 3.

```bash
Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21
```


# Birds watching

The natural scientist, Sarah, has been observing birds, and noting their sightings in a log journal.
She has given you the data in a form of list of bird IDs, as strings.  
Your task is to create two functions.
One, `find_most_frequent_bird` and `find_most_frequent_bird_count`.
Note, that the most frequent bird might be more than a single bird, eg. two different birds, AA and BB, might be observed the same number of times, and both might be the most frequent. Which one should be returned?  To solve this, Sarah decided to actually create two different implementions, one which always prints the most frequent bird that has been observed FIRST (`find_most_frequent_bird`), and one, in which one of the most frequent birds is returned, but, the order does not matter (`find_most_frequent_bird_no_order`).
* `find_most_frequent_bird_count`
   * This functions, goes over the log data, and checks which birds
     were the most frequently sighted and reports the total count of the sightings.
* `find_most_frequent_bird_no_order`
   * This function, goes over the log data and checks which birds were the most sighted.
   * Then, it reports the most sighted bird ID string.
   * If there are multiple birds that have been sighted the same number of times,
   the reported bird is picked whatever the implementation suggest is easiest.
* `find_most_frequent_bird`
   * Same as above, but this time, in case of a tie, in case there are more than a single bird with the same number of sightings,
   the bird that was observed first must be returned.


# Notes

* Use the code skeleton provided.
* All tests already there should pass. Add your own functions or tests when needed.
* Run: `cargo run --release` to test your implementation. 