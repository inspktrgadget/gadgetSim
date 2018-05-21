# growth functions in Gadget

multspec_growth <- function(temp, psi, p0, p1, p2, p3, p4, p5, p6, p7, p8) {

}

vb_growth <- function(length, linf, k, dt) {
    return((linf - length) * (1 - exp(-k * dt)))
}
