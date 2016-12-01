from operator import itemgetter


DIRECTIONS = ['L', 'R']

with open('input.txt', 'r') as f:
    movements = f.read()

movement_list = [(elem[0], int(elem[1])) for elem in movements.strip('\n ').split(', ')]

get_dir = itemgetter(0)
get_distance = itemgetter(1)

right_movements = [elem for elem in movement_list if get_dir(elem) == 'R']
left_movements = [elem for elem in movement_list if get_dir(elem) == 'L']

def distance(list_movements):
    if len(list_movements) == 4:
        d = abs(get_distance(list_movements[2]) - get_distance(list_movements[0])) + \
            abs(get_distance(list_movements[3]) - get_distance(list_movements[1]))

    if len(list_movements) == 3:
        d = abs(get_length(list_movements[2]) - get_length(list_movements[0])) + \
            get_length(list_movements[1])

    if len(list_movements) == 2:
        d  = sum(list_movements, key=get_distance)
