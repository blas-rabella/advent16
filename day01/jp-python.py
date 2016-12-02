from operator import itemgetter


DIRECTIONS = ['L', 'R']

with open('input.txt', 'r') as f:
    movements = f.read()


movement_list = [(elem[0], int(elem[1])) for elem in movements.strip('\n ').split(', ')]

get_direction = itemgetter(0)
get_distance = itemgetter(1)
x = itemgetter(0)
y = itemgetter(1)


def get_distance_point(point):

    direction = ''.join(get_direction(x(point), get_direction(y(point))))
    directions_pair = {
        'RL': (get_distance(x(point)), get_distance(y(point))),
        'LR': (-get_distance(x(point)), get_distance(y(point))),
        'LL': (-get_distance(x(point)), -get_distance(y(point))),
        'RR': (get_distance(x(point)), -get_distance(y(point)))
    }

    return directions_pair[direction]


def bloks_away(directions):
    pass
