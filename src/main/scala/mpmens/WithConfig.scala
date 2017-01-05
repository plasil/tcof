package mpmens

import mpmens.InitStages.InitStages

trait WithConfig extends Initializable {
  private[mpmens] var _config: Config = _

  private[mpmens] def _solverModel = _config.solverModel

  override private[mpmens] def _init(stage: InitStages, config: Config) = {
    super._init(stage, config)

    stage match {
      case InitStages.ConfigPropagation =>
        _config = config
      case _ =>
    }
  }
}
