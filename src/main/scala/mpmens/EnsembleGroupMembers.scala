package mpmens

class EnsembleGroupMembers[+EnsembleType <: Ensemble](values: Iterable[EnsembleType]) extends Members(values)
