#!/usr/bin/env python3

import sys
import re


PASTE_TO_CLIPBOARD = True  # TODO: Make this a CLI arg


def main():
    callstring = sys.argv[1]
    if callstring.startswith("#"):
        # Convert leadwise comp to numerical notation
        complib_string = convert_leadwise(callstring[1:])
    else:
        complib_string = convert_coursewise(callstring)

    if PASTE_TO_CLIPBOARD:
        import pyperclip

        pyperclip.copy(complib_string)
    else:
        print(complib_string)


def convert_leadwise(callstring):
    # Parse spliced: calls are in `[`s, indexed by how many leads have been generated
    method_string = ""
    call_indices = []
    call_symbols = []
    def add_call(symbol):
        call_indices.append(str(len(method_string)))
        call_symbols.append(symbol)

    is_method = True
    for seg in re.compile(r"\[([^\[\]]*)\]").split(callstring):
        if is_method:
            method_string += seg
        else:
            add_call(seg)
        is_method = not is_method

    # Add a ':' at the end of the comp
    add_call(":")

    # Create a composition string like:
    # +-----------------+----------------+---
    # |     Methods     | <call numbers> | ...
    # +-----------------+----------------+---
    # | <method_string> | <call symbols> | ...
    # +-----------------+----------------+---
    first_line = "\t".join(["Methods"] + call_indices)
    second_line = "\t".join([method_string] + call_symbols)
    return first_line + "\n" + second_line


def convert_coursewise(callstring):
    # Parse the call-string
    callpairs = []  # Pairs of (call_symbol, position)
    if "[" in callstring:
        # Parse spliced: calls are in `[`s
        regex = re.compile(r"\[([^\[\]]*)\]")
        # Alternate between methods and calls (starting with method
        method_string = ""
        is_method = True
        for seg in regex.split(callstring):
            if is_method:
                method_string += seg
            else:
                call_name = seg[:-1]
                calling_position = seg[-1]
                call_name = "-" if call_name == "" else call_name
                callpairs.append((calling_position, call_name))
            is_method = not is_method
        # Add the method string, under the `Methods` heading
        callpairs.append(("Methods", method_string))
    else:
        # Parse non-spliced: everything is a call
        call_so_far = ""
        for c in callstring:
            if not c.isalpha():
                continue
            if c.islower():
                call_so_far += c
            else:
                calling_position = c
                call_name = "-" if call_so_far == "" else call_so_far
                callpairs.append((calling_position, call_name))
                call_so_far = ""

    # Convert to complib's layout
    columns = list(set(map(lambda v: v[0], callpairs)))
    lines = ["\t".join(columns)] + [
        "\t" * columns.index(position) + call for (position, call) in callpairs
    ]
    complib_string = "\n".join(lines)
    return complib_string


if __name__ == "__main__":
    main()
