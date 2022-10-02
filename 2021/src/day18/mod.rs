use std::collections::HashSet;

use crate::time;
use crate::utils;

pub fn run(it: utils::InputType) {
    let tree = read_input(it);
    time!("Part 1", println!("{}", part1(&tree)));
    time!("Part 2", println!("{}", part2(&tree)));
}

fn part2(nums: &Vec<BinaryTree<u8>>) -> u64 {
    let mut max_mag = 0;
    for i in 0..nums.len() {
        for j in 0..nums.len() {
            if i != j {
                let mag = magnitude(&add_snailfish_numbers(&nums[i], &nums[j]));
                if mag > max_mag {
                    max_mag = mag;
                }
            }
        }
    }

    max_mag
}

fn part1(nums: &Vec<BinaryTree<u8>>) -> u64 {
    let new_num = nums
        .iter()
        .cloned()
        .reduce(|acc, item| add_snailfish_numbers(&acc, &item))
        .unwrap();

    magnitude(&new_num)
}

fn magnitude(tree: &BinaryTree<u8>) -> u64 {
    _magnitude(0, tree)
}

fn _magnitude(root_idx: usize, tree: &BinaryTree<u8>) -> u64 {
    match &tree[root_idx] {
        Node::Leaf(x) => {
            return u64::from(x.value);
        }
        Node::Branch(x) => {
            let left_mag = _magnitude(x.left, tree);
            let right_mag = _magnitude(x.right, tree);
            return 3 * left_mag + 2 * right_mag;
        }
    }
}

fn add_snailfish_numbers(a: &BinaryTree<u8>, b: &BinaryTree<u8>) -> BinaryTree<u8> {
    let mut new_num = Vec::with_capacity(a.len() + b.len() + 1);
    new_num.push(Node::Branch(Branch {
        parent: -1,
        left: 1,
        right: 0,
    }));

    let mut current_len = new_num.len();

    for (i, n) in a.iter().enumerate() {
        new_num.push(n.clone());
        match &mut new_num[i + current_len] {
            Node::Leaf(x) => {
                x.parent += isize::try_from(current_len).ok().unwrap();
            }
            Node::Branch(x) => {
                x.parent += isize::try_from(current_len).ok().unwrap();
                x.left += current_len;
                x.right += current_len;
            }
        }
    }

    current_len = new_num.len();
    if let Node::Branch(x) = &mut new_num[0] {
        x.right = current_len;
    }

    for (i, n) in b.iter().enumerate() {
        let mut new_parent = isize::try_from(current_len).ok().unwrap();
        if i == 0 {
            new_parent = 1;
        }
        new_num.push(n.clone());
        match &mut new_num[i + current_len] {
            Node::Leaf(x) => {
                x.parent += new_parent;
            }
            Node::Branch(x) => {
                x.parent += new_parent;
                x.left += current_len;
                x.right += current_len;
            }
        }
    }

    reduce_snailfish_number(&new_num)
}

fn reduce_snailfish_number(num: &BinaryTree<u8>) -> BinaryTree<u8> {
    let mut new_num = num.clone();
    loop {
        let explode_changed = _explode_snailfish_number(&mut new_num, 0, 0);

        if explode_changed {
            continue;
        }

        let split_changed = _split_snailfish_number(&mut new_num, 0);

        if !explode_changed && !split_changed {
            break;
        }
    }

    new_num
}

#[allow(dead_code)]
fn explode_snailfish_number(num: &BinaryTree<u8>) -> (BinaryTree<u8>, bool) {
    let mut new_num = num.clone();
    let changed = _explode_snailfish_number(&mut new_num, 0, 0);

    (new_num, changed)
}

fn _explode_snailfish_number(num: &mut BinaryTree<u8>, node_idx: usize, depth: usize) -> bool {
    let n = num[node_idx].clone();
    match n {
        Node::Leaf(_) => {}
        Node::Branch(x) => {
            if depth >= 3 {
                if let Node::Branch(y) = num[x.left].clone() {
                    if let Node::Leaf(z) = &num[y.left] {
                        let mut current_parent_idx = x.parent;
                        let mut current_child_idx = node_idx;
                        let mut insert_start_idx = 0;
                        while current_parent_idx != -1 {
                            let parent_idx = usize::try_from(current_parent_idx).ok().unwrap();
                            if let Node::Branch(parent_branch) = &num[parent_idx] {
                                if parent_branch.left != current_child_idx {
                                    insert_start_idx = parent_branch.left;
                                    break;
                                }
                                current_child_idx = parent_idx;
                                current_parent_idx = parent_branch.parent;
                            }
                        }

                        if current_parent_idx != -1 {
                            let left_value = z.value;
                            set_next_value_to_right(
                                num,
                                usize::try_from(insert_start_idx).ok().unwrap(),
                                left_value,
                                &combine_u8,
                            );
                        }
                    }

                    if let Node::Leaf(z) = &num[y.right] {
                        let right_value = z.value;
                        set_next_value_to_left(num, x.right, right_value, &combine_u8);
                    }

                    num[x.left] = Node::Leaf(Leaf {
                        parent: isize::try_from(node_idx).ok().unwrap(),
                        value: 0,
                    });

                    return true;
                }

                if let Node::Branch(y) = num[x.right].clone() {
                    if let Node::Leaf(z) = &num[y.left] {
                        let left_value = z.value;
                        set_next_value_to_right(num, x.left, left_value, &combine_u8);
                    }

                    if let Node::Leaf(z) = &num[y.right] {
                        let mut current_parent_idx = x.parent;
                        let mut current_child_idx = node_idx;
                        let mut insert_start_idx = 0;
                        while current_parent_idx != -1 {
                            let parent_idx = usize::try_from(current_parent_idx).ok().unwrap();
                            if let Node::Branch(parent_branch) = &num[parent_idx] {
                                if parent_branch.right != current_child_idx {
                                    insert_start_idx = parent_branch.right;
                                    break;
                                }
                                current_child_idx = parent_idx;
                                current_parent_idx = parent_branch.parent;
                            }
                        }

                        if current_parent_idx != -1 {
                            let right_value = z.value;
                            set_next_value_to_left(
                                num,
                                usize::try_from(insert_start_idx).ok().unwrap(),
                                right_value,
                                &combine_u8,
                            );
                        }
                    }

                    num[x.right] = Node::Leaf(Leaf {
                        parent: isize::try_from(node_idx).ok().unwrap(),
                        value: 0,
                    });

                    return true;
                }
            } else {
                let left_changed = _explode_snailfish_number(num, x.left, depth + 1);
                if left_changed {
                    return left_changed;
                }

                return _explode_snailfish_number(num, x.right, depth + 1);
            }
        }
    }
    false
}

fn set_next_value_to_right<T>(
    num: &mut BinaryTree<T>,
    node_idx: usize,
    val: T,
    combine_fn: &dyn Fn(&T, &T) -> T,
) {
    let mut current_node_idx = node_idx;
    loop {
        match &mut num[current_node_idx] {
            Node::Leaf(x) => {
                x.value = combine_fn(&x.value, &val);
                break;
            }
            Node::Branch(y) => {
                current_node_idx = y.right;
            }
        }
    }
}

fn set_next_value_to_left<T>(
    num: &mut BinaryTree<T>,
    node_idx: usize,
    val: T,
    combine_fn: &dyn Fn(&T, &T) -> T,
) {
    let mut current_node_idx = node_idx;
    loop {
        match &mut num[current_node_idx] {
            Node::Leaf(x) => {
                x.value = combine_fn(&x.value, &val);
                break;
            }
            Node::Branch(y) => {
                current_node_idx = y.left;
            }
        }
    }
}

#[allow(dead_code)]
fn split_snailfish_number(num: &BinaryTree<u8>) -> (BinaryTree<u8>, bool) {
    let mut new_num = num.clone();
    let changed = _split_snailfish_number(&mut new_num, 0);
    (new_num, changed)
}

fn _split_snailfish_number(num: &mut BinaryTree<u8>, node_idx: usize) -> bool {
    let n = num[node_idx].clone();
    match n {
        Node::Leaf(x) => {
            if x.value >= 10 {
                let left = x.value / 2;
                let right = if x.value % 2 == 0 { left } else { left + 1 };
                let left_index = insert_value(num, left, isize::try_from(node_idx).ok().unwrap());
                let right_index = insert_value(num, right, isize::try_from(node_idx).ok().unwrap());
                let n: Node<u8> = Node::Branch(Branch {
                    parent: x.parent,
                    left: left_index,
                    right: right_index,
                });
                num[node_idx] = n;

                return true;
            } else {
                return false;
            }
        }
        Node::Branch(x) => {
            let changed = _split_snailfish_number(num, x.left);
            if changed {
                return changed;
            }

            return _split_snailfish_number(num, x.right);
        }
    };
}

fn parse_str_to_u8(s: &str) -> u8 {
    s.parse::<u8>().unwrap()
}

#[allow(dead_code)]
fn u8_to_str(u: &u8) -> String {
    format!("{}", u)
}

fn combine_u8(a: &u8, b: &u8) -> u8 {
    a + b
}

#[allow(dead_code)]
fn read_string(s: &str) -> Vec<BinaryTree<u8>> {
    let mut nums = Vec::new();
    for l in s.lines() {
        nums.push(create_binary_tree::<u8>(l, &parse_str_to_u8))
    }

    nums
}

fn read_input(it: utils::InputType) -> Vec<BinaryTree<u8>> {
    utils::parse_file(utils::Day::Day18, it, |s| {
        create_binary_tree::<u8>(s, &parse_str_to_u8)
    })
}

#[allow(dead_code)]
fn binary_tree_to_string<T>(tree: &BinaryTree<T>, string_value: &dyn Fn(&T) -> String) -> String {
    _binary_tree_to_string(0, tree, string_value)
}

fn _binary_tree_to_string<T>(
    root_idx: usize,
    tree: &BinaryTree<T>,
    string_value: &dyn Fn(&T) -> String,
) -> String {
    match &tree[root_idx] {
        Node::Leaf(x) => {
            return string_value(&x.value);
        }
        Node::Branch(x) => {
            let left_string = _binary_tree_to_string(x.left, tree, string_value);
            let right_string = _binary_tree_to_string(x.right, tree, string_value);
            return format!("({},{})", left_string, right_string);
        }
    }
}

fn create_binary_tree<T>(tree_string: &str, parse_value: &dyn Fn(&str) -> T) -> BinaryTree<T> {
    let mut tree = Vec::new();
    insert_node::<T>(&mut tree, tree_string, -1, parse_value);

    tree
}

fn insert_node<T>(
    tree: &mut BinaryTree<T>,
    tree_string: &str,
    parent_idx: isize,
    parse_value: &dyn Fn(&str) -> T,
) -> usize {
    let parens = HashSet::from(['(', '[', '{', '<', ')', ']', '}', '>']);
    if tree_string.chars().all(|x| !parens.contains(&x)) {
        return insert_value(tree, parse_value(tree_string), parent_idx);
    } else {
        let node_idx = tree.len();
        let n: Node<T> = Node::Branch(Branch {
            parent: parent_idx,
            left: 0,
            right: 0,
        });
        tree.push(n);
        let (left, right) = parse_tree_string(tree_string);
        let left_index = insert_node(
            tree,
            left,
            isize::try_from(node_idx).ok().unwrap(),
            parse_value,
        );
        if let Node::Branch(x) = &mut tree[node_idx] {
            x.left = left_index;
        }

        let right_index = insert_node(
            tree,
            right,
            isize::try_from(node_idx).ok().unwrap(),
            parse_value,
        );
        if let Node::Branch(x) = &mut tree[node_idx] {
            x.right = right_index;
        }

        return node_idx;
    }
}

fn insert_value<T>(tree: &mut BinaryTree<T>, val: T, parent_idx: isize) -> usize {
    let size = tree.len();
    let n: Node<T> = Node::Leaf(Leaf {
        parent: parent_idx,
        value: val,
    });
    tree.push(n);
    size
}

fn parse_tree_string(tree_string: &str) -> (&str, &str) {
    let opens = HashSet::from(['(', '[', '{', '<']);
    let closes = HashSet::from([')', ']', '}', '>']);
    let mut current_depth = 0;
    let mut mid_index = 0;
    for (i, c) in tree_string.chars().enumerate() {
        if opens.contains(&c) {
            current_depth += 1;
        } else if closes.contains(&c) {
            current_depth -= 1;
        } else if c == ',' {
            if current_depth == 1 {
                mid_index = i;
                break;
            }
        }
    }

    (
        &tree_string[1..mid_index],
        &tree_string[mid_index + 1..tree_string.len() - 1],
    )
}

type BinaryTree<T> = Vec<Node<T>>;

#[derive(Debug, Clone, PartialEq)]
enum Node<T> {
    Leaf(Leaf<T>),
    Branch(Branch),
}

#[derive(Debug, Clone, PartialEq)]
struct Branch {
    parent: isize,
    left: usize,
    right: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct Leaf<T> {
    parent: isize,
    value: T,
}

#[cfg(test)]
mod tests {
    use crate::day18::binary_tree_to_string;
    use crate::day18::create_binary_tree;
    use crate::day18::explode_snailfish_number;
    use crate::day18::parse_str_to_u8;
    use crate::day18::part1;
    use crate::day18::part2;
    use crate::day18::read_input;
    use crate::day18::read_string;
    use crate::day18::split_snailfish_number;
    use crate::day18::u8_to_str;
    use crate::utils;

    #[test]
    fn split_snailfish_number_no_change() {
        let num_str = "(5,6)";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = split_snailfish_number(&num);
        assert_eq!(changed, false);
        assert_eq!(binary_tree_to_string(&new_num, &u8_to_str), num_str);
    }

    #[test]
    fn split_snailfish_number_nested_no_change() {
        let num_str = "((7,(7,8)),6)";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = split_snailfish_number(&num);
        assert_eq!(changed, false);
        assert_eq!(binary_tree_to_string(&new_num, &u8_to_str), num_str);
    }

    #[test]
    fn split_snailfish_number_change() {
        let num_str = "(15,6)";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = split_snailfish_number(&num);
        assert_eq!(changed, true);
        assert_eq!(binary_tree_to_string(&new_num, &u8_to_str), "((7,8),6)");
    }

    #[test]
    fn split_snailfish_number_nested_change() {
        let num_str = "((7,15),6)";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = split_snailfish_number(&num);
        assert_eq!(changed, true);
        assert_eq!(binary_tree_to_string(&new_num, &u8_to_str), "((7,(7,8)),6)");
    }

    #[test]
    fn split_snailfish_number_multiple_changes_possible() {
        let num_str = "((((0,7),4),(15,(0,13))),(1,1))";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = split_snailfish_number(&num);
        assert_eq!(changed, true);
        assert_eq!(
            binary_tree_to_string(&new_num, &u8_to_str),
            "((((0,7),4),((7,8),(0,13))),(1,1))"
        );
    }

    #[test]
    fn explode_snailfish_number_no_change() {
        let num_str = "((((9,8),1),2),3)";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = explode_snailfish_number(&num);
        assert_eq!(changed, false);
        assert_eq!(binary_tree_to_string(&new_num, &u8_to_str), num_str);
    }

    #[test]
    fn explode_snailfish_number_change() {
        let num_str = "(((((9,8),1),2),3),4)";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = explode_snailfish_number(&num);
        assert_eq!(changed, true);
        assert_eq!(
            binary_tree_to_string(&new_num, &u8_to_str),
            "((((0,9),2),3),4)"
        );
    }

    #[test]
    fn explode_snailfish_number_change2() {
        let num_str = "[7,[6,[5,[4,[3,2]]]]]";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = explode_snailfish_number(&num);
        assert_eq!(changed, true);
        assert_eq!(
            binary_tree_to_string(&new_num, &u8_to_str),
            "(7,(6,(5,(7,0))))"
        );
    }

    #[test]
    fn explode_snailfish_number_change3() {
        let num_str = "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]";
        let num = create_binary_tree(num_str, &parse_str_to_u8);

        let (new_num, changed) = explode_snailfish_number(&num);
        assert_eq!(changed, true);
        assert_eq!(
            binary_tree_to_string(&new_num, &u8_to_str),
            "((3,(2,(8,0))),(9,(5,(4,(3,2)))))"
        );
    }

    #[test]
    fn test_add_snailfish_numbers() {
        let input = "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]";
        let nums = read_string(input);

        let mag = part1(&nums);
        assert_eq!(mag, 3488)
    }

    #[test]
    fn part1_example() {
        let nums = read_input(utils::InputType::Example);
        assert_eq!(part1(&nums), 4140);
    }

    #[test]
    fn part1_real() {
        let nums = read_input(utils::InputType::Main);
        assert_eq!(part1(&nums), 4289);
    }

    #[test]
    fn part2_example() {
        let nums = read_input(utils::InputType::Example);
        assert_eq!(part2(&nums), 3993);
    }

    #[test]
    fn part2_real() {
        let nums = read_input(utils::InputType::Main);
        assert_eq!(part2(&nums), 4807);
    }
}
