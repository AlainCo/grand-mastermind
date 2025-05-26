<script setup lang="ts">
import type {Ref} from 'vue';
import type { Evaluation } from '../types/gametypes';
import {useGamePairLabels} from '@/services/useGamePairLabels';

const gamePairLabels=useGamePairLabels();
const props = defineProps<{
  evaluations: Ref<Evaluation[]>;
}>();

</script>
<template>
  <div class="mastermind-evaluations">
    <div v-for="(evaluation, index) in props.evaluations.value" :key="index" class="evaluation-row" >
      <div class="code">
        <div v-for="(pair, pairIndex) in evaluation.code" :key="pairIndex" class="code-pair">
          <div class="symbolshape">{{ gamePairLabels.getShapeSymbol(pair.shape) }}</div>
          <div class="symbolcolor" :style="{ color: gamePairLabels.getColorValue(pair.color) }">⏺</div>
        </div>
      </div>
      <div class="resultsblack">
        <span>{{ evaluation.nbBlack }}</span>
      </div>
      <div class="resultswhite">
        <span>{{ evaluation.nbWhite }}</span>
      </div>
      <div class="resultsblue">
        <span>{{ evaluation.nbBlue }}</span>
      </div>
    </div>
  </div>
</template>

<style scoped>
.mastermind-evaluations {
  display: flex;
  flex-direction: column;
  gap: 1px;
}

.evaluation-row {
  display: flex;
  align-items: center;
  gap: 1px;
}

.code {
  display: flex;
  gap: 1px;
}

.code-pair {
  display: flex;
  flex-direction: column;
  align-items: center;
  border: 1px solid #ccc;
  padding: 1px;
  width: 30px;
  background-color: #ccc
}

.symbolshape {
  font-size: 30px;
  margin-bottom: -8px;
  margin-top: -10px;
}

.symbolcolor {
  font-size: 23px;
  margin-top: -8px;
  margin-bottom: -6px;
}

.resultsblack {
  display: flex;
  gap: 1px;
}

.resultsblack span {
  background-color: #a0a0a0;
  padding: 1px 1px;
  border-radius: 2px;
}

.resultswhite {
  display: flex;
  gap: 1px;
}

.resultswhite span {
  background-color: #f0f0f0;
  padding: 1px 1px;
  border-radius: 2px;
}

.resultsblue {
  display: flex;
  gap: 1px;
}

.resultsblue span {
  background-color: #80c0ff;
  padding: 1px 1px;
  border-radius: 2px;
}
</style>