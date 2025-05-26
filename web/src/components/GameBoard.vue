<script setup lang="ts">
import { onMounted, watch, computed } from 'vue';
import { useRouter, useRoute } from 'vue-router';
import type { CodePair, Evaluation, Result } from '@/types/gametypes';
import GameEvaluations from "./GameEvaluations.vue"
import GameCodeInput from "./GameCodeInput.vue"
import GameResultInput from "./GameResultInput.vue"
import { useGameInitAPI } from '@/services/useGameInitAPI'
import { useGamePlayAPI } from '@/services/useGamePlayAPI'
//const { errorMessage2, code, chooseSecretRandom2, chooseSecret2 } = useGameInitAPI()
const gameInitAPI = useGameInitAPI();
const gamePlayAPI = useGamePlayAPI();

const router = useRouter()
const route = useRoute();
const mode = computed(() => route.params.mode as string);
const stage = computed(() => route.params.stage as string);

const isAttack = computed(() => mode.value === 'attack')
const isDefense = computed(() => mode.value === 'defense')
const isInit = computed(() => stage.value === 'initialize')
const isPlay = computed(() => stage.value === 'play')

const toggleStage = () => {
  const newStage = isInit.value ? 'play' : 'initialize'
  router.push(`/home/${mode.value}/${newStage}`)
}


const chooseSecretAndRefresh = async () => {
  await gameInitAPI.chooseSecret();
  await gamePlayAPI.listEvaluations();
  await gamePlayAPI.possibleCode();
};

const chooseSecretRandomAndRefresh = async () => {
  await gameInitAPI.chooseSecretRandom();
  await gamePlayAPI.listEvaluations();
  await gamePlayAPI.possibleCode();
};


const evaluateCodeAndRefresh = async () => {
  await gamePlayAPI.evaluateCode();
  await gamePlayAPI.listEvaluations();
  await gamePlayAPI.possibleCode();
};

const clearEvaluationsAndRefresh = async () => {
  await gameInitAPI.clearEvaluations();
  await gamePlayAPI.listEvaluations();
  await gamePlayAPI.possibleCode();
};

const addEvaluationAndRefresh = async () => {
  await gamePlayAPI.addEvaluation();
  await gamePlayAPI.listEvaluations();
  await gamePlayAPI.possibleCode();
};


const refreshAll = async () => {
  await gamePlayAPI.listEvaluations();
  await gamePlayAPI.possibleCode();
};

function handleUpdateResult(newResult: Result) {
  console.log('handleUpdateResult', newResult);
}

watch(gamePlayAPI.result, (newValue: Result) => {
  console.log('watchUpdateResult', newValue);
}, { deep: true });

function handleUpdateCode(newCode: CodePair[]) {
  console.log('handleUpdateCode', newCode);
  //gamePlayAPI.possibleCode();
}
watch(gamePlayAPI.code, (newValue: CodePair[]) => {
  console.log('watchUpdateCode', newValue);
  gamePlayAPI.possibleCode();
}, { deep: true });



watch(gamePlayAPI.evaluations, (newValue: Evaluation[]) => {
  console.log('watchUpdateEvaluations', newValue);
  gamePlayAPI.possibleCode();
}, { deep: true });


onMounted(() => {
  refreshAll();
})

</script>

<template>

  <div class="mastermind-board">
    <div class="mastermind-left">
      <div>
        <h1>Grand Mastermind</h1>
      </div>
      <div>
        <h1>Home</h1>
        <p>Mode: {{ mode }} isAttack: {{ isAttack }}</p>
        <p>Stage: {{ stage }} isInit; {{ isInit }} </p>
        <button @click="toggleStage">
          {{ isInit ? 'Play' : 'Initialize' }}
        </button>
      </div>

      <div v-if="isInit">
        <div>
          <h2>Init</h2>
        </div>
        <div>
          <button @click="clearEvaluationsAndRefresh">Clear Board</button>
        </div>
        <GameCodeInput :code="gameInitAPI.code.value" />
        <div>
          <button @click="gameInitAPI.getCode">Read Code</button>
        </div>        
        <div>
          <button @click="chooseSecretAndRefresh">Choose this code</button>
        </div>
        <div>
          <button @click="chooseSecretRandomAndRefresh">Choose a random code</button>
        </div>
        <div>
          <div class="errorSection">
            <div class="errorLabel">Last Init operation: </div>
            <div class="errorMessage">{{ gameInitAPI.errorMessage }}</div>
          </div>
        </div>
      </div>
      <div v-if="isPlay">
        <div>
          <h2>Play</h2>
        </div>
        <div>
          <button @click="refreshAll">Refresh Board</button>
        </div>
        <GameCodeInput :code="gamePlayAPI.code.value" @update:code="handleUpdateCode" />
        <div>
          <button @click="evaluateCodeAndRefresh">Evaluate code</button>
        </div>
        <GameResultInput :result="gamePlayAPI.result" @update:result="handleUpdateResult" />
        <div>
          <button @click="addEvaluationAndRefresh">Add Code Evaluation</button>
        </div>
        <div>
          <button @click="gamePlayAPI.possibleCode">Code is Possible?</button>
          <div>{{ gamePlayAPI.answerPossible }}</div>
        </div>
        <div>
          <button @click="gamePlayAPI.guessNearestCode">Guess Nearest Possible Code</button>
        </div>
        <div>
          <button @click="gamePlayAPI.guessFarthestCode">Guess Farthest Possible Code</button>
        </div>
        <div>
          <button @click="gamePlayAPI.guessPossibleCode">Guess Any Possible Code</button>
        </div>
        <div>
          <button @click="gamePlayAPI.guessCode">Guess secret</button>
        </div>
        <div>
          <div class="errorSection">
            <div class="errorLabel">Last Guess operation: </div>
            <div class="errorMessage">{{ gamePlayAPI.answerGuess }}</div>
          </div>
        </div>
        <div>
          <div class="errorSection">
            <div class="errorLabel">Last Play operation: </div>
            <div class="errorMessage">{{ gamePlayAPI.errorMessage }}</div>
          </div>
        </div>
      </div>
    </div>
    <div class="mastermind-right">
      <GameEvaluations :evaluations="gamePlayAPI.evaluations" />
    </div>

  </div>
</template>

<style scoped>
.mastermind-board {
  display: flex;
  flex-direction: row;
  gap: 10px;
}

.mastermind-left {
  display: flex;
  flex-direction: column;
}

.mastermind-right {
  display: flex;
  flex-direction: column;
}

.errorSection {
  display: flex;
  flex-direction: row;
}

.errorLabel {
  font-weight: bold;
}

.errorMessage {
  font-family: monospace;
  color: red;
}
</style>
