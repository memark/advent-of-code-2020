# pyright: reportWildcardImportFromLibrary=false
# pyright: reportMissingImports=false, reportUndefinedVariable=false

from collections import *
import functools
import operator
import itertools
import math
from re import *
from os import error
import re
from functools import *
import time
from typing import *


sample_1 = """
389125467
""".strip()

sample_2 = """
""".strip()


input = open('input.txt').read().strip()


def getAnswer_1(data: str) -> int:
    return getAnswer(data, isPartTwo=False)


def getAnswer_2(data: str) -> int:
    return getAnswer(data, isPartTwo=True)


def getAnswer(data: str, *, isPartTwo: bool) -> int:

    def pretty_cups():
        # Skriv om som list comprehension!
        s = list()
        for i in range(len(cups)):
            if i == curr_index:
                s.append(f'({cups[i]})')
            else:
                s.append(f'{cups[i]}')
        return ' '.join(s)

    # The cups will be arranged in a circle and labeled clockwise (your puzzle input).
    # For example, if your labeling were 32415, there would be five cups in the circle;
    # going clockwise around the circle from the first cup,
    # the cups would be labeled 3, 2, 4, 1, 5, and then back to 3 again.

    cups = [int(d) for d in data]
    L = len(cups)
    lowest_cup_label = min(cups)
    highest_cup_label = max(cups)

    # Before the crab starts, it will designate the first cup in your list as the current cup.
    curr_index = 0

    # The crab is then going to do 100 moves.
    num_moves = 4

    # Each move, the crab does the following actions:
    for m in range(1, num_moves + 1):
        print(f'-- move {m} --')
        print(f'cups: {pretty_cups()}')

        # The crab picks up the three cups
        # that are immediately clockwise of the current cup.
        # They are removed from the circle;
        # cup spacing is adjusted as necessary to maintain the circle.
        picked_up = [cups.pop((curr_index+1) % len(cups)),
                     cups.pop((curr_index+1) % len(cups)), cups.pop((curr_index+1) % len(cups))]
        print(f'pick up: {picked_up}')
        print(f'cups: {pretty_cups()}')

        # The crab selects a destination cup:
        # the cup with a label equal to the current cup's label minus one.
        # If this would select one of the cups that was just picked up,
        # the crab will keep subtracting one until it finds a cup that wasn't just picked up.
        # If at any point in this process the value goes below the lowest value on any cup's label,
        # it wraps around to the highest value on any cup's label instead.
        curr_label = cups[curr_index]
        dest_label = curr_label - 1
        # print(f'{curr_index=} {curr_label=} {dest_label=}')
        while dest_label not in cups:
            # print(f'({dest_label=} not in cups)')
            dest_label -= 1
            if dest_label < lowest_cup_label:
                dest_label = highest_cup_label
        dest_index = cups.index(dest_label)
        # print(f'{dest_index=} ')
        print(f'destination: {dest_label}')

        # The crab places the cups it just picked up
        # so that they are immediately clockwise of the destination cup.
        # They keep the same order as when they were picked up.
        # print(f'{curr_index=} {curr_label=} {dest_label=} {dest_index=}')
        cups[dest_index+1:dest_index+1] = picked_up
        print(f'cups: {pretty_cups()}')

        # The crab selects a new current cup:
        # the cup which is immediately clockwise of the current cup.
        curr_index = cups.index(curr_label) + 1
        # print(f'{curr_label=} {curr_index=}')
        print(f'cups: {pretty_cups()}')

        # Överväg att flytta upp tilldelningen av curr_index, och låta curr_label vara master.

        print()

    print(f'-- final --')
    print(f'cups: {pretty_cups()}')
    print()

    return 42


def main() -> None:
    getAndPrintAndAssertAndTimeAnswer(getAnswer_1, sample_1)
    # getAndPrintAndAssertAndTimeAnswer(getAnswer_1, sample_2)
    # getAndPrintAndAssertAndTimeAnswer(getAnswer_1, input)

    # getAndPrintAndAssertAndTimeAnswer(getAnswer_2, sample_1)
    # getAndPrintAndAssertAndTimeAnswer(getAnswer_2, sample_2)
    # getAndPrintAndAssertAndTimeAnswer(getAnswer_2, input)


def getAndPrintAndAssertAndTimeAnswer(func: Callable[[str], int], data: str, expected: Optional[int] = None) -> None:
    tic = time.perf_counter()
    answer = func(data)
    toc = time.perf_counter()

    correction = f"should be {expected}" if expected and answer != expected else ""
    print(f'{(toc-tic)*1000:.0f} ms\t{func.__qualname__} => {answer}\t\t{correction}')


if __name__ == "__main__":
    main()
