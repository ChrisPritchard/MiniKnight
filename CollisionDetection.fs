module CollisionDetection

type CheckDirection = West | East | North | South

let private tryFindVertical checker (x, y) candidates =
    let (floory, ceily) = int (floor y), int (ceil y)
    let isInVertical bx =
        let bx = float bx
        bx = x ||
        (bx < x && (bx + 1.) > x) ||
        (bx > x && (bx - 1.) < x)

    candidates |> List.tryFind (fun (bx, by) ->
            isInVertical bx && checker by ceily floory)

let private tryFindHorizontal checker (x, y) candidates =
    let (floorx, ceilx) = int (floor x), int (ceil x)
    let isInHorizontal by =
        let by = float by
        by = y ||
        (by < y && (by + 1.) > y) ||
        (by > x && (by - 1.) < y)

    candidates |> List.tryFind (fun (bx, by) ->
            isInHorizontal by && checker bx ceilx floorx)


let tryFindCollision direction (x, y) candidates =
    (x,y), candidates ||>
    match direction with
    | West -> tryFindHorizontal (fun bx ceilx _ -> bx + 1 = ceilx)
    | East -> tryFindHorizontal (fun bx _ floorx -> bx - 1 = floorx)
    | North -> tryFindVertical (fun by ceily _ -> by = ceily - 1)
    | South -> tryFindVertical (fun by _ floory -> by = floory + 1)