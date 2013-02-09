

sphere =
    let a = (2*pi)/16
        steps = take 16 $ iterate (+a) (-pi)
        xs = map sin steps
        ys = map cos steps
    in zip xs ys
