package mpmens

import mpmens.InitStages.InitStages

trait Initializable {
  private[mpmens] def _init(stage: InitStages, config: Config): Unit = {
  }
}



