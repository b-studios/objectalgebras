package objectalgebras

trait Algebra[Sig[-I0, +O]] {
	type Complete[E] = Sig[E, E]
}

trait Algebra1[Sig[-I1, -I0, +O]] {
}

trait Algebra2[Sig[-I2, -I1, -I0, +O]] {
}
