CASH = [500, 200, 100, 50, 20, 10, 5, 2, 1]


def money_print(a: int, b: int) -> None:
    "Sert à montrer les billets de manière correcte à l'écran."
    final_string = f"{b} billet(s) de {a} euros(s).\n" if a >= 5 else f"{b} pièce(s) de {a} euros(s).\n"
    print("" if b == 0 else final_string, end="")


montant = int(input("Veuillez insérer un montant: "))
res = {} 

for BILLET in CASH:
    nombre_billet, montant = divmod(montant, BILLET)
    res[BILLET] = nombre_billet

for a, b in res.items():
    money_print(a, b)
