def getCharacterCounts(s):
    charCounts = dict()
    for char in s:
        if char in charCounts:
            charCounts[char] += 1
        else:
            charCounts[char] = 1
    return charCounts

def challenge1(filename):
    """Does challenge1

    >>> challenge1('input.txt')
    7163
    """
    num2s = 0
    num3s = 0
    with open(filename) as f:
        for line in f:
            charCounts = getCharacterCounts(line.rstrip()).values()
            if 2 in charCounts:
                num2s += 1
            
            if 3 in charCounts:
                num3s += 1
    return num2s * num3s

def getCommonString(s1, s2):
    """Check if two string are 1 character different and return common string
    >>> getCommonString('abcde', 'axcde')
    'acde'

    >>> getCommonString("abcd", "abcdef")
    ''

    >>> getCommonString("axcde", "ybcde")
    ''
    """
    mismatchCount = 0
    mismatchIndex = -1
    if abs(len(s1) - len(s2)) > 1:
        return ''
    
    for i in range(0, len(s1) - 1):
        if s1[i] != s2[i]:
            mismatchCount += 1
            mismatchIndex = i
        if mismatchCount > 1:
            break

    return s1[:mismatchIndex] + s1[mismatchIndex + 1:] if mismatchCount == 1 else ''

def challenge2(filename):
    with open(filename) as f:
        strings = [line.rstrip() for line in f]
    
    solution = ''
    for string1 in strings:
        for string2 in strings:
            commonString = getCommonString(string1, string2)
            if commonString != '':
                solution = commonString
                break
        if solution != '':
            break
    return solution


#print(challenge1('input.txt'))
print(challenge2('input.txt'))

if __name__ == "__main__":
    import doctest
    doctest.testmod()