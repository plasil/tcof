package mpmens

class EnsembleGroupMembers[+EnsembleType <: Ensemble](values: Iterable[EnsembleType]) extends Members(values) with WithConfig {
  private[mpmens] def mapEnsembleActivationRecursive(thisGroup: EnsembleGroup[Ensemble], parentGroup: EnsembleGroup[Ensemble] = null, ensembleIndexInParent: Int = 0): Unit = {
    if (parentGroup != null) {
      for (idx <- 0 until size) {
        _solverModel.ifThen(_solverModel.member(idx, thisGroup.allMembersVar), _solverModel.member(ensembleIndexInParent, parentGroup.allMembersVar))
      }
    }

    var idx = 0
    for (ensemble <- values) {
      for (group <- ensemble._ensembleGroups.values) {
        group.allMembers.mapEnsembleActivationRecursive(group, thisGroup, idx)
      }

      idx = idx + 1
    }
  }
}
