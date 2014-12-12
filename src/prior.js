var ex=[{p:1},{p:1},{p:3},{p:3},{p:3},
        {p:5},{p:5},{p:8},{p:8},{p:8}]
         
function maxProb (xs) {
  var acc=0
  for (v of xs) acc+=1/v.p
  return 1/acc
}

function rndIdx (xs) {
  var mp=maxProb(xs)
  var r=Math.random()
  for (i in xs) { 
    var ap=r-mp/xs[i].p
    if (ap<=0) return i
  }
  return i
}
