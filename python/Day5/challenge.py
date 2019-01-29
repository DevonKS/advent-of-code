from string import ascii_lowercase

def challenge1(filename):
    """
    challenge 1

    >>> challenge1("input.txt")
    9348
    """
    polymer = ""
    with open(filename) as f:
        polymer = f.read().strip()

    result = polymer[0]
    previousChar = result
    for char in polymer[1:]:
        if char.lower() == previousChar.lower() and char != previousChar:
            result = result[:-1]
        else:
            result += char
        
        previousChar = result[-1] if result != "" else "" 
    
    return len(result)

def challenge2(filename):
    """
    challenge 2

    >>> challenge2("input.txt")
    4996
    """
    polymer = ""
    with open(filename) as f:
        polymer = f.read().strip()

    optimisedPolymers = []
    for char in ascii_lowercase:
        newPolymer = polymer.replace(char, "").replace(char.upper(), "")

        result = newPolymer[0]
        previousChar = result
        for char in newPolymer[1:]:
            if char.lower() == previousChar.lower() and char != previousChar:
                result = result[:-1]
            else:
                result += char
            
            previousChar = result[-1] if result != "" else "" 
        optimisedPolymers.append(result)
    return len(min(optimisedPolymers, key=(lambda item: len(item))))

if __name__ == "__main__":
    import doctest
    doctest.testmod()