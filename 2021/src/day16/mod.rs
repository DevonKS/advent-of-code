use core::panic;

use crate::{time, utils};

pub fn run(it: utils::InputType) {
    let packet = read_input(it);
    time!("Part 1", println!("{}", part1(&packet)));
    time!("Part 2", println!("{}", part2(&packet)));
}

fn part2(packet: &Packet) -> u64 {
    calculate_packet(packet)
}

fn part1(packet: &Packet) -> u64 {
    let mut total: u64 = 0;

    let mut stack = Vec::new();
    stack.push(packet);

    while stack.len() > 0 {
        let current_p = stack.pop().unwrap();
        total += u64::from(current_p.version);
        if current_p.sub_packets.is_some() {
            for p in current_p.sub_packets.as_ref().unwrap() {
                stack.push(&p)
            }
        }
    }

    total
}

fn calculate_packet(packet: &Packet) -> u64 {
    match packet.type_id {
        0 => {
            let mut total = 0;
            for p in packet.sub_packets.as_ref().unwrap() {
                total += calculate_packet(&p)
            }
            total
        }
        1 => {
            let mut total = 1;
            for p in packet.sub_packets.as_ref().unwrap() {
                total *= calculate_packet(&p)
            }
            total
        }
        2 => {
            let mut min = u64::MAX;
            for p in packet.sub_packets.as_ref().unwrap() {
                let packet_value = calculate_packet(&p);
                if packet_value < min {
                    min = packet_value;
                }
            }
            min
        }
        3 => {
            let mut max = u64::MIN;
            for p in packet.sub_packets.as_ref().unwrap() {
                let packet_value = calculate_packet(&p);
                if packet_value > max {
                    max = packet_value;
                }
            }
            max
        }
        4 => packet.literal_value.unwrap(),
        5 => {
            let sub_packets = packet.sub_packets.as_ref().unwrap();
            if calculate_packet(&sub_packets[0]) > calculate_packet(&sub_packets[1]) {
                return 1;
            } else {
                return 0;
            }
        }
        6 => {
            let sub_packets = packet.sub_packets.as_ref().unwrap();
            if calculate_packet(&sub_packets[0]) < calculate_packet(&sub_packets[1]) {
                return 1;
            } else {
                return 0;
            }
        }
        7 => {
            let sub_packets = packet.sub_packets.as_ref().unwrap();
            if calculate_packet(&sub_packets[0]) == calculate_packet(&sub_packets[1]) {
                return 1;
            } else {
                return 0;
            }
        }
        _ => panic!("Unsupported Type Id. Packet: {:?}.", packet),
    }
}

fn read_input(it: utils::InputType) -> Packet {
    let str_input = utils::read_file(utils::Day::Day16, it);
    read_string(str_input.trim())
}

fn read_string(s: &str) -> Packet {
    let mut bits = Vec::with_capacity(s.len() * 4);
    for c in s.chars() {
        let binary_str = format!("{:04b}", u8::from_str_radix(&c.to_string(), 16).unwrap());
        for bc in binary_str.chars() {
            bits.push(bc)
        }
    }
    parse_packet(&bits)
}

fn parse_packet(bits: &Vec<char>) -> Packet {
    let (packet, _) = parse_one_packet(bits, 0);
    packet
}

fn parse_one_packet(bits: &Vec<char>, mut current_idx: usize) -> (Packet, usize) {
    let version_bits = &bits[current_idx..current_idx + 3];
    current_idx += 3;
    let version = u8::from_str_radix(&version_bits.iter().collect::<String>(), 2).unwrap();

    let type_id_bits = &bits[current_idx..current_idx + 3];
    current_idx += 3;
    let type_id = u8::from_str_radix(&type_id_bits.iter().collect::<String>(), 2).unwrap();

    let mut packet = Packet {
        version,
        type_id,
        literal_value: None,
        sub_packets: None,
    };

    if type_id == 4 {
        let mut done = false;
        let mut str_value = String::new();
        while !done {
            if bits[current_idx] == '0' {
                done = true;
            }

            for i in current_idx + 1..current_idx + 5 {
                str_value.push(bits[i])
            }
            current_idx += 5
        }
        packet.literal_value = Some(u64::from_str_radix(&str_value, 2).unwrap());
    } else {
        let mut sub_packets = Vec::new();
        let length_type_id = bits[current_idx];
        current_idx += 1;
        if length_type_id == '0' {
            let num_bit_of_sub_packets_bits = &bits[current_idx..current_idx + 15];
            current_idx += 15;
            let num_bit_of_sub_packets =
                usize::from_str_radix(&num_bit_of_sub_packets_bits.iter().collect::<String>(), 2)
                    .unwrap();
            let mut bits_parsed: usize = 0;
            while bits_parsed < num_bit_of_sub_packets {
                let (p, i) = parse_one_packet(bits, current_idx);
                bits_parsed += i - current_idx;
                current_idx = i;
                sub_packets.push(p);
            }
        } else if length_type_id == '1' {
            let num_of_sub_packets_bits = &bits[current_idx..current_idx + 11];
            current_idx += 11;
            let num_of_sub_packets =
                usize::from_str_radix(&num_of_sub_packets_bits.iter().collect::<String>(), 2)
                    .unwrap();
            for _ in 0..num_of_sub_packets {
                let (p, i) = parse_one_packet(bits, current_idx);
                current_idx = i;
                sub_packets.push(p);
            }
        } else {
            panic!(
                "invalid length_type_id. Bits: \"{:?}\", Current Index: \"{}\"",
                bits, current_idx
            );
        }
        packet.sub_packets = Some(sub_packets);
    }

    (packet, current_idx)
}

#[derive(Debug)]
struct Packet {
    version: u8,
    // FIXME Should I have a different type for the different type_ids?
    type_id: u8,
    literal_value: Option<u64>,
    sub_packets: Option<Vec<Packet>>,
}

#[cfg(test)]
mod tests {
    use crate::day16::part1;
    use crate::day16::part2;
    use crate::day16::read_input;
    use crate::day16::read_string;
    use crate::utils;

    #[test]
    fn part1_example() {
        let packet = read_input(utils::InputType::Example);
        assert_eq!(part1(&packet), 9);
    }

    #[test]
    fn part1_example2() {
        let packet = read_string("EE00D40C823060");
        assert_eq!(part1(&packet), 14);
    }

    #[test]
    fn part1_example3() {
        let packet = read_string("8A004A801A8002F478");
        println!("{:?}", packet);
        assert_eq!(part1(&packet), 16);
    }

    #[test]
    fn part1_example4() {
        let packet = read_string("620080001611562C8802118E34");
        assert_eq!(part1(&packet), 12);
    }

    #[test]
    fn part1_example5() {
        let packet = read_string("C0015000016115A2E0802F182340");
        assert_eq!(part1(&packet), 23);
    }

    #[test]
    fn part1_example6() {
        let packet = read_string("A0016C880162017C3686B18A3D4780");
        assert_eq!(part1(&packet), 31);
    }

    #[test]
    fn part1_real() {
        let packet = read_input(utils::InputType::Main);
        assert_eq!(part1(&packet), 963);
    }

    #[test]
    fn part2_example() {
        let packet = read_input(utils::InputType::Example);
        assert_eq!(part2(&packet), 1);
    }

    #[test]
    fn part2_example2() {
        let packet = read_string("C200B40A82");
        assert_eq!(part2(&packet), 3);
    }

    #[test]
    fn part2_example3() {
        let packet = read_string("04005AC33890");
        assert_eq!(part2(&packet), 54);
    }

    #[test]
    fn part2_example4() {
        let packet = read_string("880086C3E88112");
        assert_eq!(part2(&packet), 7);
    }

    #[test]
    fn part2_example5() {
        let packet = read_string("CE00C43D881120");
        assert_eq!(part2(&packet), 9);
    }

    #[test]
    fn part2_example6() {
        let packet = read_string("D8005AC2A8F0");
        assert_eq!(part2(&packet), 1);
    }

    #[test]
    fn part2_example7() {
        let packet = read_string("F600BC2D8F");
        assert_eq!(part2(&packet), 0);
    }

    #[test]
    fn part2_example8() {
        let packet = read_string("9C005AC2F8F0");
        assert_eq!(part2(&packet), 0);
    }

    #[test]
    fn part2_example9() {
        let packet = read_string("9C0141080250320F1802104A08");
        assert_eq!(part2(&packet), 1);
    }

    #[test]
    fn part2_real() {
        let packet = read_input(utils::InputType::Main);
        assert_eq!(part2(&packet), 1549026292886);
    }
}
