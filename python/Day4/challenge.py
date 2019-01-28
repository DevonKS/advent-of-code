from datetime import datetime

def parseLine(line):
    """
    parses a line.

    >>> parseLine("[1518-04-21 00:57] wakes up")
    {'timeString': '1518-04-21 00:57', 'time': datetime.datetime(1518, 4, 21, 0, 57), 'action': 'wakes', 'guard-id': -1}
    >>> parseLine("[1518-09-03 00:12] falls asleep")
    {'timeString': '1518-09-03 00:12', 'time': datetime.datetime(1518, 9, 3, 0, 12), 'action': 'asleep', 'guard-id': -1}
    >>> parseLine("[1518-04-21 00:04] Guard #3331 begins shift")
    {'timeString': '1518-04-21 00:04', 'time': datetime.datetime(1518, 4, 21, 0, 4), 'action': 'shift', 'guard-id': 3331}
    """
    parts = line.split("] ")
    timeString = parts[0].replace("[", "")
    time = datetime.strptime(timeString, "%Y-%m-%d %H:%M")
    actionString = parts[1]
    if actionString.startswith("wakes"):
        action = "wakes"
    elif actionString.startswith("falls"):
        action = "asleep"
    else:
        action = "shift"
    guardId = -1 if action != "shift" else int(actionString.split(" ")[1].replace("#", ""))
    return {"timeString": timeString, "time": time, "action": action, "guard-id": guardId}

def challenge1(filename):
    """
    challenge1

    >>> challenge1("input.txt")
    36898
    """
    lines = []
    with open(filename) as f:
        lines = [parseLine(line) for line in f]

    lines.sort(key=(lambda item: item["time"]))

    values = {"current-guard": -1, "previous-time": -1}
    for line in lines:
        currentTime = int(line["timeString"].split(":")[1])
        if line["action"] == "shift":
            values["current-guard"] = line["guard-id"]
        else:
            if values["current-guard"] not in values:
                values[values["current-guard"]] = {"awake": [], "asleep": []}

            values[values["current-guard"]]["awake" if line["action"] == "asleep" else "asleep"].extend(range(values["previous-time"], currentTime))

        values["previous-time"] = currentTime
    del values["current-guard"]
    del values["previous-time"]

    mostAsleepGuardId = max(values.keys(), key=(lambda key: len(values[key]["asleep"])))
    guardAsleepMinutes = values[mostAsleepGuardId]["asleep"]
    mostAsleepMinute = max(set(guardAsleepMinutes), key=guardAsleepMinutes.count)
    return mostAsleepGuardId * mostAsleepMinute

def challenge2(filename):
    """
    challenge2

    >>> challenge2("input.txt")
    80711
    """
    lines = []
    with open(filename) as f:
        lines = [parseLine(line) for line in f]

    lines.sort(key=(lambda item: item["time"]))

    values = {"current-guard": -1, "previous-time": -1}
    for line in lines:
        currentTime = int(line["timeString"].split(":")[1])
        if line["action"] == "shift":
            values["current-guard"] = line["guard-id"]
        else:
            if values["current-guard"] not in values:
                values[values["current-guard"]] = {"awake": [], "asleep": []}

            values[values["current-guard"]]["awake" if line["action"] == "asleep" else "asleep"].extend(range(values["previous-time"], currentTime))

        values["previous-time"] = currentTime
    del values["current-guard"]
    del values["previous-time"]

    mostAsleepGuardId = max(values.keys(), key=(lambda key: max(set(values[key]["asleep"]), key=values[key]["asleep"].count)))
    guardAsleepMinutes = values[mostAsleepGuardId]["asleep"]
    mostAsleepMinute = max(set(guardAsleepMinutes), key=guardAsleepMinutes.count)
    return mostAsleepGuardId * mostAsleepMinute

if __name__ == "__main__":
    import doctest
    doctest.testmod()