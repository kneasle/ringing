#!/usr/bin/env python3

# Python script to generate release notes for the latest Monument release from `CHANGLELOG.md`.



# Read CHANGELOG file
md_lines = open("CHANGELOG.md").read().split("\n")

# Extract the first non-'unreleased' section
has_started_section = False
section = ""
for l in md_lines:
    is_horizontal_rule = all([c == "-" for c in l]) and l != ""
    if is_horizontal_rule:
        continue

    # Count number of `#`s which start the line, giving us the heading number
    heading_num = 0
    for c in l:
        if c != "#":
            break
        heading_num += 1

    # Release sections are headed by <h2> elements
    if heading_num == 2 and "nreleased" not in l:
        if has_started_section:
            break
        else:
            has_started_section = True
            continue

    # Don't include the header for Monument
    if heading_num == 3 and "Monument" in l:
        continue

    # Reduce any headers by two levels
    if heading_num >= 3:
        l = l[2:]

    # If all other checks fail, include this line in the release notes
    if has_started_section:
        section += l + "\n"

# Send `section` to stdout to be used as release notes
print(section.strip())
