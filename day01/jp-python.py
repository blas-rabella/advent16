import operator
from operator import itemgetter
from functools import reduce


get_direction = itemgetter(0)
get_distance = itemgetter(1)
x = itemgetter(0)
y = itemgetter(1)


DIRECTIONS = {
    'N': (0, 1),
    'S': (0, -1),
    'E': (1, 0),
    'W': (-1, 0)
}


def get_orientation(mov, orientation):
    op = getattr(operator, 'add')\
        if get_direction(mov) == 'R' else getattr(operator, 'sub')

    return op(orientation, 1) % 4


def bloks_away(movements):
    block = (0, 0)
    orientation = 0
    for mov in movements:
        print('Actual block: ', block)
        print('Mov: ', mov)
        orientation = get_orientation(mov, orientation)
        block = make_move(block, mov, orientation)
        print('New block: ', block)
        print('-' * 90 + '\n')

    return abs(x(block)) + abs(y(block))


def make_move(d_point, point, direction):
    cardinal_points = ['N', 'E', 'S', 'W']
    orientation = cardinal_points[direction]
    return (x(d_point) + x(DIRECTIONS[orientation]) * int(get_distance(point)),
            y(d_point) + y(DIRECTIONS[orientation]) * int(get_distance(point)))


if __name__ == '__main__':
    with open('input.txt', 'r') as f:
        movements = f.read()
    # movements = [
    #     'R4', 'R3', 'R5', 'L3', 'L5', 'R2', 'L2', 'R5', 'L2', 'R5', 'R5', 'R5', 'R1', 'R3', 'L2', 'L2',
    #     'L1', 'R5', 'L3', 'R1', 'L2', 'R1', 'L3', 'L5', 'L1', 'R3', 'L4', 'R2', 'R4', 'L3', 'L1', 'R4',
    #     'L4', 'R3', 'L5', 'L3', 'R188', 'R4', 'L1', 'R48', 'L5', 'R4', 'R71', 'R3', 'L2', 'R188',
    #     'L3', 'R2', 'L3', 'R3', 'L5', 'L1', 'R1', 'L2', 'L4', 'L2', 'R5', 'L3', 'R3', 'R3', 'R4', 'L3',
    #     'L4', 'R5', 'L4', 'L4', 'R3', 'R4', 'L4', 'R1', 'L3', 'L1', 'L1', 'R4', 'R1', 'L4', 'R1', 'L1',
    #     'L3', 'R2', 'L2', 'R2', 'L1', 'R5', 'R3', 'R4', 'L5', 'R2', 'R5', 'L5', 'R1', 'R2', 'L1', 'L3',
    #     'R3', 'R1', 'R3', 'L4', 'R4', 'L4', 'L1', 'R1', 'L2', 'L2', 'L4', 'R1', 'L3', 'R4', 'L2', 'R3',
    #     'L1', 'L5', 'R4', 'R5', 'R2', 'R5', 'R1', 'R5', 'R1', 'R3', 'L3', 'L2', 'L2', 'L5', 'R2', 'L2',
    #     'R5', 'R5', 'L2', 'R3', 'L5', 'R5', 'L2', 'R4', 'R2', 'L1', 'R3', 'L5', 'R3', 'R2', 'R5', 'L1',
    #     'R3', 'L2', 'R2', 'R1'
    # ]
    movements = [mov for mov in movements.strip('\n').split(', ')]

    print(bloks_away(movements))
