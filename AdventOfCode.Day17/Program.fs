open System
open System.IO
open System.Text.RegularExpressions

[<EntryPoint>]
let main _ =
    let (xMin, xMax, yMin, yMax) =
        File.ReadAllText "Input.txt"
        |> (fun text -> 
            let matches = Regex(@"x=(-?\d+)\.\.(-?\d+), y=(-?\d+)\.\.(-?\d+)").Match(text)
            let values = matches.Groups.Values |> Seq.map (fun group -> group.Value) |> Seq.toArray
            (values.[1] |> Int32.Parse, values.[2] |> Int32.Parse, values.[3] |> Int32.Parse, values.[4] |> Int32.Parse))

    let (x0, y0) = (0,0)
    
    // Initial position = (0,0)
    // Initial velocity = (vx, vy)
    // 
    // Assume vx_0 > 0 and vy_0 > 0
    //
    // Propagation: 
    //  x_n  = x_n-1 + vx_n-1
    //  y_n  = x_n-1 + vy_n-1
    //  vx_n = max(vx_n-1 - 1, 0)
    //  vy_n = vy_n-1 - 1

    let motionX vx n =
        // The x-position stays stationary after vx steps (velocity in x-direction is 0 then)
        if n > vx then
            x0 + vx * (vx+1) / 2
        else
            x0 + vx * n - ((n-1) * n) / 2

    let motionY vy n =
        y0 + n * vy - ((n-1) * n) / 2
        
    let isTargetHit vx vy n =
        let x = motionX vx n
        let y = motionY vy n
        
        if xMin <= x && x <= xMax && yMin <= y && y <= yMax then
            //printfn "target hit: x = %d; y = %d; vx = %d; vy = %d; n = %d" x y vx vy n
            Some (x, y, vx, vy, n)
        else
            None

    // The maximum initial velocity in x-direction is xMax.
    // Higher initial velocities overshoot the target area after the first step.
    //
    // The maximum initial velocity in y-direction is -(yMin - 1).
    // According to the equations of motion, the trajectory in y-direction always returns to y0 with velocity -(vy + 1) at step n = 2*vy + 1.
    // The probe will overshoot the target in y-direction latest after -yMin / (vy + 1) further steps (bad approximation, assuming probe does not accelerate any more).
    // Higher initial velocities overshoot the target area after passing y0.

    // Some bad estimations to make life easier:
    // - maximum steps: approx with (2*vy + 1) - yMin
    // - minimum steps: approx with 0

    let targetHits = 
        let vxMin = 1
        let vxMax = xMax
        let vyMin = yMin
        let vyMax = -(yMin - 1)

        seq {
            for vx in vxMin .. vxMax do
                for vy in vyMin .. vyMax do
                    // Approximating number of steps as follows
                    // - in y-direction
                    //      * if vy > 0, then approximate as above by going from second y0 crossing
                    //      * if vy <= 0, then approximate by -yMin (bad approx)
                    // - in x-direction
                    //      * vx >= 1 (bad approx)
                    //      * vx <= xMax
                    //
                    // Math.Max approximations for n in y- and x- directions to account for all target hits.

                    let nMin = 1
                    let nMaxForY = 
                        if vy > 0 then (2*vy + 1) - yMin
                        else -yMin
                    let nMaxForX = xMax

                    yield! seq {nMin .. Math.Max(nMaxForX, nMaxForY)} |> Seq.choose (isTargetHit vx vy)
        }
        |> Seq.toArray

    let distinctInitialValues = targetHits |> Seq.map (fun (_,_,vx,vy,_) -> (vx,vy)) |> Seq.distinct |> Seq.toArray
    let maxVy = targetHits |> Seq.map (fun (_,_,_,vy,_) -> vy) |> Seq.max
    let maxY = motionY maxVy (maxVy+1)

    printfn ""
    printfn "found %d target hits" targetHits.Length
    printfn "distinct initial velocities = %d" distinctInitialValues.Length
    printfn "maximum vy = %d" maxVy
    printfn "maximum y  = %d" maxY

    0