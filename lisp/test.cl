def len(xs) (
    if eq(xs [])
        0
    else
        add(1 len(tail(xs)))
)

def concat(xs ys) (
    if eq(ys [])
        xs
    else do
        let y head(ys)
        let ys tail(ys)
        let xs append(xs y)
        concat(xs ys)
)

def return(x) (x)

def _sort(xs ys) (
    let x head(xs)
    let xs tail(xs)
    let y head(xs)
    if gt(x y)


)
def sort(xs) (_sort(xs []))
