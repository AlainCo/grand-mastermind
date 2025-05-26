import { ref , reactive } from 'vue';
import axios from 'axios';
import type {CodePair,Result,Evaluation} from '@/types/gametypes'

export function useGamePlayAPI() {

  //const codeLength = 4;
  const mastermindAPI=import.meta.env.VITE_MASTERMIND_URL_BASE;
  const getUrlAPI = (api: string): string => {
    return mastermindAPI + api;
  }

  const errorMessage = ref<string>("");
  const answerPossible = ref<string>("")
  const answerGuess = ref<string>("")
  
  const code = ref(reactive<CodePair[]>([...[
    { color: 'red', shape: 'square' },
    { color: 'green', shape: 'rectangle' },
    { color: 'blue', shape: 'triangle' },
    { color: 'yellow', shape: 'hexagon' }
  ]] ));
  const result=ref(reactive<Result>({nbBlack: 0, nbWhite: 0, nbBlue: 0, }));
  const evaluations = ref<Evaluation[]>([]);

// attack mode
  const evaluateCode = async () => {
    try {
      console.log('evaluateCode');
      errorMessage.value = "Waiting";
      const dataIn = { code: code.value };
      const response = await axios.post(getUrlAPI('/evaluate_code'), dataIn, {
        headers: {
          'Content-Type': 'application/json',
        }
      });
      if (response.data) {
        const status: number = response.data.status;
        console.log('evaluateCode status=', status);
        if (status == 0) {
          errorMessage.value = "OK";
          result.value.nbBlack = response.data.nbBlack;
          result.value.nbWhite = response.data.nbWhite;
          result.value.nbBlue = response.data.nbBlue;
          console.log('evaluated code', result.value);
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error computing evaluation:', error)
    }
  }

  // defense mode
  const addEvaluation = async () => {
    try {
      console.log('addEvaluation');
      errorMessage.value = "Waiting";
      const evaluation:Evaluation={code: code.value,nbBlack:result.value.nbBlack,nbWhite:result.value.nbWhite,nbBlue:result.value.nbBlue};
      const dataIn = { evaluation: evaluation };
      const response = await axios.post(getUrlAPI('/add_evaluation'), dataIn, {
        headers: {
          'Content-Type': 'application/json',
        }
      });
      if (response.data) {
        const status: number = response.data.status;
        console.log('addEvaluation status=', status);
        if (status == 0) {
          errorMessage.value = "OK";
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error choosing secret:', error)
    }
  }

  // guess
  
  const guessCode = async () => {
    try {
      errorMessage.value = "Waiting";
      answerGuess.value="...";
      console.log('guessCode');
      const response = await axios.post(getUrlAPI('/guess_code'));
      if (response.data) {
        const status: number = response.data.status;
        console.log('guessCode status=', status);
        if (status == 0) {
          console.log('guessCode data=', response.data)
          console.log('guessCode data.code=', response.data.code)
          code.value = response.data.code;
          errorMessage.value = "OK";
          answerGuess.value = "Propose Guess";
        } else {
          errorMessage.value = "Error#" + status;
          answerGuess.value="Failure";
        }
      } else {
        errorMessage.value = "Failure";
        answerGuess.value="Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error fetching evaluations:', error);
      answerGuess.value="Failure";
    }
  }
  
  
  const guessNearestCode = async () => {
    try {
      console.log('guessNearestCode');
      errorMessage.value = "Waiting";
      answerGuess.value = "...";
      const dataIn = { code: code.value };
      const response = await axios.post(getUrlAPI('/guess_nearest_code'), dataIn, {
        headers: {
          'Content-Type': 'application/json',
        }
      });
      if (response.data) {
        const status: number = response.data.status;
        console.log('guessNearestCode status=', status);
        if (status == 0) {
          errorMessage.value = "OK";
          answerGuess.value = "Found Nearest";
          console.log('guessNearestCode data=', response.data)
          console.log('guessNearestCode data.code=', response.data.code)
          code.value = response.data.code;
        } else if (status == 1) {
          errorMessage.value = "OK";
          answerGuess.value = "No Possible Found";
        } else if (status == 2) {
          errorMessage.value = "OK";
          answerGuess.value = "Too Long";
        } else {
          errorMessage.value = "Error#" + status;
          answerGuess.value = "Failure";
        }
      } else {
        errorMessage.value = "Failure";
        answerGuess.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error choosing secret:', error);
      answerGuess.value = "Failure";
    }
  }
  
  const guessFarthestCode = async () => {
    try {
      console.log('guessFarthestCode');
      errorMessage.value = "Waiting";
      answerGuess.value = "...";
      const dataIn = { code: code.value };
      const response = await axios.post(getUrlAPI('/guess_farthest_code'), dataIn, {
        headers: {
          'Content-Type': 'application/json',
        }
      });
      if (response.data) {
        const status: number = response.data.status;
        console.log('guessFarthestCode status=', status);
        if (status == 0) {
          errorMessage.value = "OK";
          answerGuess.value = "Found Farthest";
          console.log('guessFarthestCode data=', response.data)
          console.log('guessFarthestCode data.code=', response.data.code)
          code.value = response.data.code;
        } else if (status == 1) {
          errorMessage.value = "OK";
          answerGuess.value = "No Possible Found";
        } else if (status == 2) {
          errorMessage.value = "OK";
          answerGuess.value = "Too Long";
        } else {
          errorMessage.value = "Error#" + status;
          answerGuess.value = "Failure";
        }
      } else {
        errorMessage.value = "Failure";
        answerGuess.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error choosing secret:', error);
      answerGuess.value = "Failure";
    }
  }
  

  const guessPossibleCode = async () => {
    try {
      console.log('guessPossibleCode');
      errorMessage.value = "Waiting";
      answerGuess.value = "...";
      const response = await axios.post(getUrlAPI('/guess_possible_code'));
      if (response.data) {
        const status: number = response.data.status;
        console.log('guessPossibleCode status=', status);
        if (status == 0) {
          errorMessage.value = "OK";
          answerGuess.value = "Found Possible";
          console.log('guessNearestCode data=', response.data)
          console.log('guessNearestCode data.code=', response.data.code)
          code.value = response.data.code;
        } else if (status == 1) {
          errorMessage.value = "OK";
          answerGuess.value = "No Possible Found";
        } else if (status == 2) {
          errorMessage.value = "OK";
          answerGuess.value = "Too Long";
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error choosing secret:', error)
    }
  }

  // all mode


  const listEvaluations = async () => {
    try {
      errorMessage.value = "Waiting";
      console.log('listEvaluations');
      const response = await axios.get(getUrlAPI('/list_evaluations'));
      if (response.data) {
        const status: number = response.data.status;
        console.log('listEvaluations status=', status);
        if (status == 0) {
          evaluations.value = response.data.evaluations;
          errorMessage.value = "OK";
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error fetching evaluations:', error)
    }
  }
  

  const possibleCode = async () => {
    try {
      console.log('possibleCode');
      errorMessage.value = "Waiting";
      answerPossible.value = "...";
      const dataIn = { code: code.value };
      const response = await axios.post(getUrlAPI('/possible_code'), dataIn, {
        headers: {
          'Content-Type': 'application/json',
        }
      });
      if (response.data) {
        const status: number = response.data.status;
        console.log('possibleCode status=', status);
        if (status == 0) {
          errorMessage.value = "OK";
          answerPossible.value = "Possible";
        } else if (status == 1) {
          errorMessage.value = "OK";
          answerPossible.value = "Impossible";
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error choosing secret:', error)
    }
  }
  


  return {
    errorMessage: errorMessage,
    code: code,
    result:result,
    evaluations:evaluations,
    answerPossible:answerPossible,
    answerGuess:answerGuess,
    evaluateCode:evaluateCode,
    listEvaluations:listEvaluations,
    possibleCode:possibleCode,
    addEvaluation:addEvaluation,
    guessCode:guessCode,
    guessNearestCode:guessNearestCode,
    guessFarthestCode:guessFarthestCode,
    guessPossibleCode:guessPossibleCode,
 }
}