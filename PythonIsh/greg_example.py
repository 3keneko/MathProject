from math import sqrt

# J'importe la fonction sqrt, permettant de calculer les racines carrées.
def second_degre(
    a, b, c
) -> float:  # Je défini une fonction qui me permettra en mettant uniquement les valeurs de a, b, c de résoudre une équation du second degré du type ax^2 + bx + c=0
    p = b ** 2 - 4 * a * c  # p est ici le déterminant de mon équation.
    if (
        p > 0
    ):  # If permet la création de conditions, si le déterminant est supérieur à 0, nous aurons deux racines.
        x1 = (-b + sqrt(p)) / 2 * a
        x2 = (-b - sqrt(p)) / 2 * a
        return (
            f"les valeurs sont {x1} et {x2}"
        )  # j'affiche les valeurs du x1 et du x2 à l'écran.
    elif p == 0:  # Si P = 0, il n'y a qu'une seule solution.
        x = -b / (2 * a)
        return (
            f"la valeur est égale à {x}"
        )  # j'affiche la valeur de x grâce au return et à la fonction f string
    return "il n'y a pas de valeur de x"


a = int(input("introduire une valeur de a :"))
b = int(input("introduire une valeur de b :"))
c = int(input("introduire une valeur de c :"))
# La fonction input a pour but de demander une valeur à l'utilisateur du programme..
# La fonction int pour convertir la chaîne de caractères obtenues en nombre.
print(second_degre(a, b, c))
