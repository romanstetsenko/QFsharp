open System

type CashFlow = CashFlow of float
type Rate = Rate of float
type Period = Period of int

type PresentValue = PresentValue of CashFlow * Rate * Period
type NetPresentValue = NetPresentValue of PresentValue seq

module Regular =
    let calcPV (PresentValue((CashFlow cf), (Rate r), (Period p))) =
            cf / Math.Pow(1. + r, float p)

    let calcNPV (NetPresentValue pvs) = 
        pvs |> Seq.sumBy calcPV     


let cf1 = CashFlow 1500.
let r = Rate 0.07
let p1 = Period 1

let pv1 = PresentValue (cf1,r, p1)


let config = 
    Seq.init (Int32.MaxValue/50) id
    |> Seq.map (function | 0 -> (CashFlow -5000., r, Period 0) | i -> (CashFlow 1500., r, Period i))
    |> Seq.map PresentValue
    |> NetPresentValue

Regular.calcNPV config

