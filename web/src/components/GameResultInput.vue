<script setup lang="ts">
import { defineProps,watch,computed } from 'vue';
import type { Result } from '../types/gametypes';
import type {Ref} from 'vue';
import 'vue-select/dist/vue-select.css'


const props = defineProps<{
  result: Ref<Result>;
  readonly?: boolean;
}>();

const isReadOnly = computed(() => props.readonly ?? false);

const resultState = props.result;
const emit = defineEmits<{ (e: 'update:result', value: Result): void; }>();

watch(resultState, (newValue:Result) => {
    emit('update:result', newValue);
    console.log("update:result",newValue);
}, { deep: true });

function logChange(value: Object): void {
    console.log('logChange',value)
    console.log('resultState', resultState );
    console.log('result', props.result.value );
    console.log('readonly',isReadOnly);
}

</script>

<template>
    <div class="mastermind-input">
        <div class="results">
            <div class="resultsblack">
                <input type="number" v-model.number="resultState.nbBlack" min="0" max="4" step="1"
                    @input="logChange" :disabled="isReadOnly">
            </div>
            <div class="resultswhite">
                <input type="number" v-model.number="resultState.nbWhite" min="0" max="4" step="1"
                    @input="logChange" :disabled="isReadOnly">
            </div>
            <div class="resultsblue">
                <input type="number" v-model.number="resultState.nbBlue" min="0" max="4" step="1"
                    @input="logChange" :disabled="isReadOnly">
            </div>
        </div>
    </div>
</template>

<style scoped>
.mastermind-input {
    display: flex;
    align-items: center;
    gap: 1px;
}

.results {
    display: flex;
    flex-direction: column;
    gap: 1px;
}

.resultsblack {
    display: flex;
    gap: 1px;
}

.resultsblack input {
    background-color: #a0a0a0;
    padding: 1px 1px;
    border-radius: 2px;
}

.resultswhite {
    display: flex;
    gap: 1px;
}

.resultswhite input {
    background-color: #f0f0f0;
    padding: 1px 1px;
}

.resultsblue {
    display: flex;
    gap: 1px;
}

.resultsblue input {
    background-color: #80c0ff;
    padding: 1px 1px;
}
</style>
