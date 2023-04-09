type najemnik = string
type lastnik = string
type vrt = Obdelovan of lastnik
| Oddan of lastnik * (vrt * vrt list)
| Prost

let vrt_vrt =  (Obdelovan "Galois", [Prost])

let vrt_primer = Oddan("Kovalevskaya", (Obdelovan "Galois", [Obdelovan "Lagrange"; Prost]))

let oddajalec_vrta vrt = 
  match vrt with 
  | Obdelovan(_) -> None 
  | Oddan(lastnik, _) -> Some lastnik 
  | Prost -> None 
