def parseLine(line):
    """
    Parses a line and returns a map of the clain properties.

    >>> parseLine("#1 @ 829,837: 11x22")
    {'id': '1', 'x': '829', 'y': '837', 'width': '11', 'height': '22'}
    >>> parseLine("#34 @ 35,270: 11x25")
    {'id': '34', 'x': '35', 'y': '270', 'width': '11', 'height': '25'}
    """
    parts = line.split(' ')
    id = parts[0].replace('#', '')
    startCoordinatesParts = parts[2].split(',')
    startXCoordinate = startCoordinatesParts[0]
    startYCoordinate = startCoordinatesParts[1].replace(':', '')
    sizeParts = parts[3].split('x')
    width = sizeParts[0]
    height = sizeParts[1]
    return {"id": id, "x": startXCoordinate, "y": startYCoordinate, "width": width, "height": height}

print(parseLine("#1 @ 829,837: 11x22"))

if __name__ == "__main__":
    import doctest
    doctest.testmod()