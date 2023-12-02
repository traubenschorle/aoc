MAX_CUBES = {
    "blue": 14,
    "green": 13,
    "red": 12,
}

def main():
    with open('input.txt') as f:
        lines = f.read().splitlines() 

    impossible_games = list()

    for line in lines:
        game, draws = line.split(":")
        game_id = int(game.split(" ")[1])

        for draw in draws.split("; "):
            colors = draw.strip().split(", ")
            for color in colors:
                amount, kind = color.split(" ")
                if int(amount) > MAX_CUBES[kind]:
                    # impossible game
                    impossible_games.append(game_id)
                    continue


    print(sum(i for i in range(1, 101) if i not in impossible_games))

if __name__ == "__main__":
    main()