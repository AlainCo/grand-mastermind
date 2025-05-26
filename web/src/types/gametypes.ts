
export interface CodePair {
  color: string
  shape: string
};

export interface Result {
  nbBlack: number
  nbWhite: number
  nbBlue: number
};
export interface Evaluation {
  code: CodePair[]
  nbBlack: number
  nbWhite: number
  nbBlue: number
};

export interface CodeLine {
  code: CodePair[]
};
