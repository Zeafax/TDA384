import random

def generate_map(size):
    map_grid = []
    for _ in range(size):
        row = []
        for _ in range(size):
            row.append(random.choices(['.', '#'], weights=[7, 3])[0])
        map_grid.append(' '.join(row))
    return '\n'.join(map_grid)

def save_map_to_file(map_data, filename):
    with open(filename, 'w') as file:
        file.write(map_data)

sizes = [100, 300]
for size in sizes:
    map_data = generate_map(size)
    filename = f'{size}x{size}.map'
    save_map_to_file(map_data, filename)