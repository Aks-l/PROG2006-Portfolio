// Lab 08

fn main() {
    test_reverse();

    test_oldest_students_count();

    test_find_most_frequent_bird_count();
    test_find_most_frequent_bird(find_most_frequent_bird);
    test_find_most_frequent_bird(find_most_frequent_bird_no_order);

    test_find_most_frequent_bird_with_order();
    test_find_most_frequent_bird_no_order()
}

fn reverse(input: &str) -> String {
    let mut reversed = String::new();
    for c in input.chars(){
        reversed.insert(0, c);
    }
    reversed
}

fn count_oldest_students(data: &str) -> u64 {
    let mut oldest = (0,0);
    for student in data.lines() {
        // get last part of the line
        let age = student.split_whitespace().last().unwrap().parse::<u64>().unwrap();
        let (maxage, _) = oldest;
        if age > maxage {
            oldest = (age, 1);
        } else if age == maxage {
            oldest.1 += 1;
        }
    }
    oldest.1
}

fn find_most_frequent_bird_count(birds: &Vec<&str>) -> u64 {
    let mut bird_freq = std::collections::HashMap::new();
    for bird in birds {
        *bird_freq.entry(bird).or_insert(0) += 1;
    }
    return bird_freq.values().max().unwrap_or(&0).clone()
}

fn find_most_frequent_bird_no_order<'a>(birds: &[&'a str]) -> Option<&'a str> {
    use std::collections::HashMap;

    if birds.is_empty() {
        return None;
    }

    let mut freq = HashMap::new();
    for &bird in birds {
        *freq.entry(bird).or_insert(0) += 1;
    }

    freq.into_iter()
        .max_by_key(|&(_, count)| count)
        .map(|(bird, _)| bird)
}


fn find_most_frequent_bird<'a>(birds: &[&'a str]) -> Option<&'a str> {
    use indexmap::IndexMap;
    if birds.is_empty() {
        return None;
    }
    let mut freq = IndexMap::new();
    for &bird in birds {
        *freq.entry(bird).or_insert(0) += 1;
    }
    //iterate through the indexmap and return the first with the max freq
    let mut max_freq = 0;
    let mut most_frequent_bird = None;
    for (bird, count) in freq {
        if count > max_freq {
            max_freq = count;
            most_frequent_bird = Some(bird);
        }
    }
    most_frequent_bird

}

/////////////////////////////// Tests //////////////////////////////////
// Run all the tests with `cargo run --release` to see if everything worked.

// Test cases for the reverse function.
fn test_reverse() {
    let data = [
        ("", ""),
        ("a", "a"), // edge cases
        ("Hello", "olleH"),
        ("World", "dlroW"), // Hellow World, of course :)
        ("1234567890", "0987654321"),
        ("123456789", "987654321"),
        ("This is my string", "gnirts ym si sihT"),
        ("This\tis my\n string", "gnirts \nym si\tsihT"), // with tabs and newlines
    ];
    for (input, expected) in data {
        let output = reverse(input);
        assert_eq!(output, expected);
    }
}

// Test cases for the oldest_students_count function.
fn test_oldest_students_count() {
    let data = r#"Alice Cooper 25
Alice Boa 23
Bob Marley 23
Alice Chains 25
Charlie Brown 21
Charlie Chaplin 25
Eve Wonder 24
Sandra White 21"#;
    let output = count_oldest_students(data);
    assert_eq!(output, 3);
}

// Normal test for just the count of the most frequent bird.
fn test_find_most_frequent_bird_count() {
    let data = [
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1"], 3),
        (
            vec!["zz", "a1", "zz", "bz2", "a3", "a1", "bz2", "a1", "a1"],
            4,
        ),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1"], 5),
        (
            vec!["zz", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1"],
            6,
        ),
        (
            vec![
                "zz", "bz2", "bz2", "a1", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1",
            ],
            7,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            8,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            9,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            10,
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1",
            ],
            11,
        ),
        (
            vec![
                "bz2", "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1", "a1", "a1",
            ],
            12,
        ),
    ];
    for (input, expected) in data {
        let output = find_most_frequent_bird_count(&input);
        assert_eq!(output, expected);
    }
}

// Normal test cases that are independent on the order of the most frequent bird appearance
// in the data log.
fn test_find_most_frequent_bird(f: for<'a> fn(&[&'a str]) -> Option<&'a str>) {
    let data = [
        (vec![], None),           // edge case
        (vec!["a1"], Some("a1")), // edge case
        (
            vec!["bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1"],
            Some("a1"),
        ),
        (vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1"], Some("a1")),
        (
            vec!["bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1"],
            Some("a1"),
        ),
        (
            vec!["a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1"],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "bz2", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1",
            ],
            Some("a1"),
        ),
        (
            vec![
                "bz2", "a1", "bz2", "a3", "a1", "bz2", "a1", "a1", "a1", "a1", "a1", "a1", "a1",
                "a1", "a1", "a1",
            ],
            Some("a1"),
        ),
    ];

    for (input, expected) in data {
        let output = f(&input);
        assert_eq!(output, expected);
    }
}

// Test cases that test if the function returns the most frequent bird without keeping the order.
// Think, what makes this implementation easier than the one with order?
fn test_find_most_frequent_bird_no_order() {
    let data = [
        (vec![], vec![None]),           // edge case
        (vec!["a1"], vec![Some("a1")]), // edge case
        (
            vec!["a1", "a2", "a3"],
            vec![Some("a1"), Some("a2"), Some("a3")],
        ), // edge case, must return the first most frequent
        (
            vec!["a1", "a2", "a3", "a2", "a3", "a1"],
            vec![Some("a1"), Some("a2"), Some("a3")],
        ), // edge case, must return the first most frequent
        (
            vec!["a2", "a1", "a1", "a2", "a2", "a3", "a1"],
            vec![Some("a2"), Some("a1")],
        ),
        (
            vec!["a1", "a2", "a2", "a1", "a2", "a1"],
            vec![Some("a1"), Some("a2")],
        ),
        (
            vec!["a1", "a2", "a3", "a3", "a3", "a2", "a2"],
            vec![Some("a2"), Some("a3")],
        ), // edge case, must return the first most frequent
    ];

    for (input, expected) in data {
        let output = find_most_frequent_bird_no_order(&input);
        // Debugging output in case we make an error in test cases
        // println!("{:?} is in {:?}", output, expected);
        assert!(expected.contains(&output));
    }
}

// Test cases that are dependent on the order of the most frequent bird appearance
// in the data log. These test if the spec has been completed properly.
// Think, what makes this implementation different, or harder, from the one that the order is not important?
fn test_find_most_frequent_bird_with_order() {
    let data = [
        (vec![], None),                                         // edge case
        (vec!["a1"], Some("a1")),                               // edge case
        (vec!["a1", "a2", "a3"], Some("a1")), // edge case, must return the first most frequent
        (vec!["a1", "a2", "a3", "a2", "a3", "a1"], Some("a1")), // edge case, must return the first most frequent
        (vec!["a2", "a1", "a1", "a2", "a2", "a3"], Some("a2")),
        (vec!["a1", "a2", "a2", "a1", "a2", "a1"], Some("a1")),
        (vec!["a1", "a2", "a3", "a3", "a3", "a2", "a2"], Some("a2")), // edge case, must return the first most frequent
    ];

    for (input, expected) in data {
        let output = find_most_frequent_bird(&input);
        assert_eq!(output, expected);
    }
}

