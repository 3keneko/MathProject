from math import sqrt

def second_degre(
    a, b, c
) -> float:  
    p = b ** 2 - 4 * a * c  
    if (
        p > 0
    ):  
        x1 = (-b + sqrt(p)) / 2 * a
        x2 = (-b - sqrt(p)) / 2 * a
        return (
            f"les valeurs sont {x1} et {x2}"
        )  
    elif p == 0:  
        x = -b / (2 * a)
        return (
            f"la valeur est égale à {x}"
        )  
    return "il n'y a pas de valeur de x"

a = int(input("introduire une valeur de a :"))
b = int(input("introduire une valeur de b :"))
c = int(input("introduire une valeur de c :"))

print(second_degre(a, b, c))
