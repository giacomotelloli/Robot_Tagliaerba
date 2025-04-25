import pygame
import re

# === CONFIG ===
CELL_SIZE = 80
GRID_SIZE = 6
WINDOW_SIZE = GRID_SIZE * CELL_SIZE
FPS = 2

# === COLORI ===
COLOR_GRASS = (34, 139, 34)
COLOR_CUT = (210, 180, 140)
COLOR_ROBOT = (255, 255, 0)
COLOR_CHARGER = (0, 191, 255)
COLOR_OBSTACLE = (105, 105, 105)
COLOR_GRID = (0, 0, 0)
COLOR_BG = (255, 255, 255)

def cell_to_coord(cell: str):
    return int(cell[1]) - 1, int(cell[2]) - 1

def parse_init_from_pddl(pddl_path: str):
    with open(pddl_path, 'r') as f:
        content = f.read().lower()

    # Estrai la sezione :init
    init_block = re.search(r"\(:init(.*?)\)\s*\)", content, re.DOTALL)
    if not init_block:
        raise ValueError("Sezione :init non trovata.")
    init_text = init_block.group(1)

    robot_position = None
    charging_stations = set()
    grass_cells = set()
    obstacles = set()

    # Trova tutti i predicati
    for match in re.findall(r"\(([^)]+)\)", init_text):
        if match.startswith("at robot1"):
            cell = re.search(r"c\d\d", match).group()
            robot_position = cell_to_coord(cell)
        elif match.startswith("charging-station"):
            cell = re.search(r"c\d\d", match).group()
            charging_stations.add(cell_to_coord(cell))
        elif match.startswith("has-grass"):
            cell = re.search(r"c\d\d", match).group()
            grass_cells.add(cell_to_coord(cell))
        elif match.startswith("obstacle"):
            cell = re.search(r"c\d\d", match).group()
            obstacles.add(cell_to_coord(cell))
        elif match.startswith("not (has-grass"):
            cell = re.search(r"c\d\d", match).group()
            grass_cells.discard(cell_to_coord(cell))

    print("DEBUG — Stato iniziale PDDL:")
    print("  • Robot in:", robot_position)
    print("  • Stazioni di ricarica:", charging_stations)
    print("  • Ostacoli:", obstacles)
    print("  • Celle con erba:", grass_cells)

    return robot_position, charging_stations, grass_cells, obstacles

def parse_plan_from_file(filepath: str):
    steps = []
    with open(filepath, 'r') as file:
        for line in file:
            if not line.strip():
                continue
            parts = line.strip().split()
            action = parts[0]
            if action.startswith("cut") or action.startswith("charge"):
                coord = cell_to_coord(parts[2])
            elif action == "move":
                coord = cell_to_coord(parts[3])
            else:
                continue
            steps.append((action, coord))
    return steps

def draw_grid(screen, grass, robot, chargers, obstacles):
    for row in range(GRID_SIZE):
        for col in range(GRID_SIZE):
            rect = pygame.Rect(col * CELL_SIZE, row * CELL_SIZE, CELL_SIZE, CELL_SIZE)

            if (row, col) in obstacles:
                color = COLOR_OBSTACLE
            elif (row, col) == robot:
                color = COLOR_ROBOT
            elif (row, col) in chargers:
                color = COLOR_CHARGER
            elif (row, col) in grass:
                color = COLOR_GRASS
            else:
                color = COLOR_CUT

            pygame.draw.rect(screen, color, rect)
            pygame.draw.rect(screen, COLOR_GRID, rect, 1)

def animate_from_pddl(pddl_file: str, plan_file: str):
    robot_pos, chargers, grass, obstacles = parse_init_from_pddl(pddl_file)
    plan = parse_plan_from_file(plan_file)

    pygame.init()
    screen = pygame.display.set_mode((WINDOW_SIZE, WINDOW_SIZE))
    pygame.display.set_caption("Animazione Robot Tagliaerba")
    clock = pygame.time.Clock()

    running = True
    i = 0
    while running and i < len(plan):
        for event in pygame.event.get():
            if event.type == pygame.QUIT:
                running = False

        action, target = plan[i]
        if action.startswith("move"):
            robot_pos = target
        elif action.startswith("cut"):
            grass.discard(target)

        screen.fill(COLOR_BG)
        draw_grid(screen, grass, robot_pos, chargers, obstacles)
        pygame.display.flip()
        clock.tick(FPS)
        i += 1

    pygame.quit()

if __name__ == '__main__':
    # Cambia i nomi se i tuoi file si chiamano diversamente
    animate_from_pddl('instance2_classic.pddl', 'robot_plan.txt')
