{-# LANGUAGE OverloadedStrings #-}

module Data.Scene where

import Data.Aeson

import Data.Vectors

type Color = Vec3

data SceneObject = Sphere { position :: Vec3,
                            radius :: Float,
                            materialId :: String
                          } deriving (Read, Show)

data Material = BlinnPhongMaterial { id :: String,
                                     ambient :: Color,
                                     diffuse :: Color,
                                     specular :: Color,
                                     shininess :: Float
                                   } deriving (Read, Show)

data Scene = Scene { materials :: [ Material ],
                     objects :: [ SceneObject ]
                   } deriving (Read, Show)

