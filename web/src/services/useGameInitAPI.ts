import { ref , reactive } from 'vue';
import axios from 'axios';
import type {CodePair} from '@/types/gametypes'

export function useGameInitAPI() {

  //const codeLength = 4;
  const mastermindAPI=import.meta.env.VITE_MASTERMIND_URL_BASE;
  const getUrlAPI = (api: string): string => {
    return mastermindAPI + api;
  }

  const errorMessage = ref<string>("");
  const code = ref(reactive<CodePair[]>([...[
      { color: 'red', shape: 'square' },
      { color: 'green', shape: 'rectangle' },
      { color: 'blue', shape: 'triangle' },
      { color: 'yellow', shape: 'hexagon' }
    ]] ));


  const chooseSecretRandom = async () => {
    try {
      console.log('chooseSecretRandom');
      errorMessage.value = "Waiting";
      const response = await axios.post(getUrlAPI('/choose_secret_random'));
      if (response.data) {
        const status: number = response.data.status;
        console.log('chooseSecretRandom status=', status);
        if (status == 0) {
          code.value = response.data.code;
          errorMessage.value = "OK";
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error choosing random secret:', error)
    }
  }

  const getCode = async () => {
    try {
      errorMessage.value = "Waiting";
      console.log('getCode');
      const response = await axios.get(getUrlAPI('/get_code'));
      if (response.data) {
        const status: number = response.data.status;
        console.log('getCode status=', status);
        if (status == 0) {
          console.log('getCode data=', response.data);
          console.log('getCode data.code=', response.data.code);
          if(response.data.code) {
            code.value = response.data.code;
            errorMessage.value = "OK";
          } else {
            errorMessage.value = "No Code Set";
          }
        } else {
          errorMessage.value = "Error#" + status;
        }
      } else {
        errorMessage.value = "Failure";
      }
    } catch (error) {
      errorMessage.value = "Failure:" + error;
      console.error('Error fetching code:', error);
    }
  }
  
  const chooseSecret = async () => {
    try {
      console.log('chooseSecret');
      errorMessage.value = "Waiting";
      const dataIn = { code: code };
      const response = await axios.post(
        getUrlAPI('/choose_secret'), 
        dataIn, 
        {
          headers: {
            'Content-Type': 'application/json'
          }
        });
      if (response.data) {
        const status: number = response.data.status;
        console.log('chooseSecret status=', status);
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

  const clearEvaluations = async () => {
    try {
      console.log('clearEvaluations');
      errorMessage.value = "Waiting";
      const response = await axios.post(getUrlAPI('/clear_evaluations'));
      if (response.data) {
        const status: number = response.data.status;
        console.log('clearEvaluations status=', status);
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
      console.error('Error clearing evaluations:', error)
    }
  }
  return {
    errorMessage: errorMessage,
    code: code,
    clearEvaluations:clearEvaluations,
    chooseSecretRandom: chooseSecretRandom,
    chooseSecret: chooseSecret,
    getCode:getCode
  }
}