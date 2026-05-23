package domain

import monads.Reader

def recipeOf(product: Product): Reader[Config, Recipe] =
  Reader(_.recipes(product))

def productionCost(product: Product, quantity: Int): Reader[Config, Int] =
  Reader { config =>
    val recipe = config.recipes(product)
    val costPerUnit = recipe.materials.map { case (m, qty) =>
      config.materialCosts(m) * qty
    }.sum
    costPerUnit * quantity
  }

def canProduce(product: Product, quantity: Int, available: Map[Material, Int]): Reader[Config, Boolean] =
  Reader { config =>
    val recipe = config.recipes(product)
    recipe.materials.forall { case (m, need) =>
      available.getOrElse(m, 0) >= need * quantity
    }
  }

def isDefectAllowed(currentDefectRate: Double): Reader[Config, Boolean] =
  Reader(_.allowedDefectRate >= currentDefectRate)