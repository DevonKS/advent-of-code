def challenge1(filename):
    with open(filename) as f:
        nums = [int(line) for line in f]
        return sum(nums)

def challenge2(filename):
    with open(filename) as f:
        nums = [int(line) for line in f]
        frequencies_seen = set()
        current_frequency = 0
        duplicate_frequency_found = False
        index = 0
        while not duplicate_frequency_found:
            current_frequency += nums[index % len(nums)]
            if current_frequency in frequencies_seen:
                duplicate_frequency_found = True
            frequencies_seen.add(current_frequency)
            index += 1
        return current_frequency if duplicate_frequency_found else 'Not Found!'

#print(challenge1('input.txt'))
print(challenge2('input.txt'))