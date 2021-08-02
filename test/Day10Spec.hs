module Day10Spec where

import Day10
import Test.Hspec

run :: IO ()
run = hspec $ do
  let jolts = input "./input/Day10p1.txt"
  let jolts' = input "./input/Day10p1test.txt"
  let jolts'' = input "./input/Day10p1test2.txt"
  
  describe "input" $ do
    it "input" $ do
      let expected = [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22]
      jolts' `shouldBe` expected

  describe "diffs" $ do
    it "testcases" $ do
      let expected = [1, 3, 1, 1, 1, 3, 1, 1, 3, 1, 3, 3]
      diffs jolts' `shouldBe` expected
              
  describe "part1" $ do
    it "testcases" $ do
      part1 jolts' `shouldBe` 35
      part1 jolts'' `shouldBe` 220

    it "puzzle" $ do
      part1 jolts `shouldBe` 2664

  describe "arrangements" $ do
    it "testcases" $ do
      let expected =
            [
              [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 12, 15, 16, 19, 22]
            ]
      arrangements jolts' `shouldBe` expected

  describe "valid" $ do
    it "testcases" $ do
      let toBeTested =
            [
              [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 12, 15, 16, 19, 22]
            ]
      all valid toBeTested `shouldBe` True

  describe "combinations" $ do
    it "testcases" $ do
      combinations 0 [] `shouldBe` [[]]
      combinations 0 [1..3] `shouldBe` [[]]
      combinations 1 [1..3] `shouldBe` [[1],[2],[3],[]]
      combinations 2 [1..3] `shouldBe` [[1,2],[1,3],[1],[2,3],[2],[3],[]]
      combinations 3 [1..3] `shouldBe` [[1,2,3],[1,2],[1,3],[1],[2,3],[2],[3],[]]

  describe "allPaths" $ do
    it "testcases" $ do
      let expected =
            [
              [0, 1, 4, 5, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 5, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 6, 7, 10, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 11, 12, 15, 16, 19, 22],
              [0, 1, 4, 7, 10, 12, 15, 16, 19, 22]
            ]
      let tree =
            Node 0 [
              Node 1 [
                Node 4 [
                  Node 5 [
                    Node 6 [
                      Node 7 [
                        Node 10 [
                          Node 11 [
                            Node 12 [
                              Node 15 [
                                Node 16 [
                                  Node 19 [
                                    Node 22 []
                                  ]
                                ]
                              ]
                            ]
                          ],
                          Node 12 [
                            Node 15 [
                              Node 16 [
                                Node 19 [
                                  Node 22 []
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ],
                    Node 7 [
                      Node 10 [
                        Node 11 [
                          Node 12 [
                            Node 15 [
                              Node 16 [
                                Node 19 [
                                  Node 22 []
                                ]
                              ]
                            ]
                          ]
                        ],
                        Node 12 [
                          Node 15 [
                            Node 16 [
                              Node 19 [
                                Node 22 []
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ],
                  Node 6 [
                    Node 7 [
                      Node 10 [
                        Node 11 [
                          Node 12 [
                            Node 15 [
                              Node 16 [
                                Node 19 [
                                  Node 22 []
                                ]
                              ]
                            ]
                          ]
                        ],
                        Node 12 [
                          Node 15 [
                            Node 16 [
                              Node 19 [
                                Node 22 []
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ],
                  Node 7 [
                    Node 10 [
                      Node 11 [
                        Node 12 [
                          Node 15 [
                            Node 16 [
                              Node 19 [
                                Node 22 []
                              ]
                            ]
                          ]
                        ]
                      ],
                      Node 12 [
                        Node 15 [
                          Node 16 [
                            Node 19 [
                              Node 22 []
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
      allPaths [] [] tree `shouldBe` expected

  describe "makeTree" $ do
    it "testcases" $ do
      let expected =
            Node 0 [
              Node 1 [
                Node 4 [
                  Node 5 [
                    Node 6 [
                      Node 7 [
                        Node 10 [
                          Node 11 [
                            Node 12 [
                              Node 15 [
                                Node 16 [
                                  Node 19 [
                                    Node 22 []
                                  ]
                                ]
                              ]
                            ]
                          ],
                          Node 12 [
                            Node 15 [
                              Node 16 [
                                Node 19 [
                                  Node 22 []
                                ]
                              ]
                            ]
                          ]
                        ]
                      ]
                    ],
                    Node 7 [
                      Node 10 [
                        Node 11 [
                          Node 12 [
                            Node 15 [
                              Node 16 [
                                Node 19 [
                                  Node 22 []
                                ]
                              ]
                            ]
                          ]
                        ],
                        Node 12 [
                          Node 15 [
                            Node 16 [
                              Node 19 [
                                Node 22 []
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ],
                  Node 6 [
                    Node 7 [
                      Node 10 [
                        Node 11 [
                          Node 12 [
                            Node 15 [
                              Node 16 [
                                Node 19 [
                                  Node 22 []
                                ]
                              ]
                            ]
                          ]
                        ],
                        Node 12 [
                          Node 15 [
                            Node 16 [
                              Node 19 [
                                Node 22 []
                              ]
                            ]
                          ]
                        ]
                      ]
                    ]
                  ],
                  Node 7 [
                    Node 10 [
                      Node 11 [
                        Node 12 [
                          Node 15 [
                            Node 16 [
                              Node 19 [
                                Node 22 []
                              ]
                            ]
                          ]
                        ]
                      ],
                      Node 12 [
                        Node 15 [
                          Node 16 [
                            Node 19 [
                              Node 22 []
                            ]
                          ]
                        ]
                      ]
                    ]
                  ]
                ]
              ]
            ]
      makeTree jolts' 0 `shouldBe` expected

  describe "countPaths" $ do
    it "testcases" $ do
      countPaths jolts' (head jolts') (last jolts') 0 `shouldBe` 8
      countPaths jolts'' (head jolts'') (last jolts'') 0 `shouldBe` 19208

  describe "part2" $ do
    it "testcases" $ do
      part2 jolts' `shouldBe` 8
      part2 jolts'' `shouldBe` 19208

    it "puzzle" $ do
      part2 jolts `shouldBe` 1
