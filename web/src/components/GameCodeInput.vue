<script setup lang="ts">
import { defineProps, watch, computed } from 'vue';
import type { CodePair } from '../types/gametypes';
import vSelect from 'vue-select'
import 'vue-select/dist/vue-select.css'
import {useGamePairLabels} from '@/services/useGamePairLabels';

const gamePairLabels=useGamePairLabels();

interface ColorOption {
    color: string
    colorCode: string
};
interface ShapeOption {
    shape: string
    shapeSymbol: string
};


const colorOptions: ColorOption[] = gamePairLabels.getColorList().map((color) => ({ color: color, colorCode: gamePairLabels.getColorValue(color) }))
const shapeOptions: ShapeOption[] = gamePairLabels.getShapeList().map((shape) => ({ shape: shape, shapeSymbol: gamePairLabels.getShapeSymbol(shape) }))


const props = defineProps<{
    code: CodePair[];
    readonly?: boolean;
}>();
const isReadOnly = computed(() => props.readonly ?? false);



const emit = defineEmits<{ (e: 'update:code', value: CodePair[]): void; }>();

watch(props.code, (newValue: CodePair[]) => {
    emit('update:code', newValue);
    console.log("update:code", newValue);
}, { deep: true });

function logChange(value: Object): void {
    console.log('logChange', value)
    console.log('code', props.code);
}

</script>

<template>
    <div class="mastermind-input">
        <div class="code">
            <div v-for="(pair, pairIndex) in code" :key="pairIndex" class="code-pair">
                <v-select class="symbolshape custom-v-select" v-model="pair.shape" :options="shapeOptions"
                    :reduce="(option: ShapeOption) => option.shape" :clearable="false" :disabled="isReadOnly"
                    @option:selected="logChange">
                    <template #option="{ shape, shapeSymbol }">
                        <div class="shape-option">
                            <span class="shape-symbol">{{ shapeSymbol }}</span>
                            <span class="shape-label">{{ shape }}</span>
                        </div>
                    </template>
                    <template #selected-option="{ shapeSymbol }">
                        <div class="shape-option-selected">
                            <span class="shape-label">{{ shapeSymbol }}</span>
                        </div>
                    </template>
                </v-select>
                <v-select class="symbolcolor custom-v-select" v-model="pair.color" :options="colorOptions"
                    :reduce="(option: ColorOption) => option.color" :clearable="false" :disabled="isReadOnly"
                    @option:selected="logChange">
                    <template #option="{ color, colorCode }">
                        <div class="color-option">
                            <span class="color-dot" :style="{ color: colorCode }">⏺</span>
                            <span class="color-label" :style="{ color: colorCode }">{{ color }}</span>
                        </div>
                    </template>
                    <template #selected-option="{ colorCode }">
                        <div class="color-option-selected">
                            <span class="color-dot" :style="{ color: colorCode }">⏺</span>
                        </div>
                    </template>
                </v-select>
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
    width: 35px;
    height: 90px;
    background-color: #ccc
}

.symbolshape {
    font-size: 30px;
    margin-bottom: 5px;
    margin-top: 1px;
}

.symbolcolor {
    font-size: 30px;
    margin-top: 0px;
    margin-bottom: 0px;
}

.color-option-selected {
    font-size: 10px;
}

.custom-v-select :deep(.vs__dropdown-toggle) {
    display: flex;
    flex-direction: column;
    padding: 0 0px;
}

.custom-v-select :deep(.vs__selected) {
    margin-top: -8px;
    margin-bottom: -8px;
    margin-left: -2px;
    margin-right: -2px;
    padding: 0;
}

.custom-v-select :deep(.vs__selected-options) {
    margin: 0;
    padding: 0;
}

.custom-v-select :deep(.vs__search) {
    margin: 0;
    padding: 0;
}

.custom-v-select :deep(.vs__actions) {
    padding: 0;
    margin: 0;
    align-self: center;
}


.custom-v-select :deep(.vs__spinner) {
    padding: 0;
    margin: 0;
    font-size: 3px;
    border: 0;
}

.shape-option {
    display: flex;
    align-items: center;
    padding: 4px 0;
}

.color-option {
    display: flex;
    align-items: center;
    padding: 4px 0;
}

.shape-symbol {
    font-size: 30px;
    margin: 0px;
}

.color-dot {
    font-size: 30px;
    margin: 0px;
}

.color-label {
    font-size: 0.9em;
}

.shape-label {
    font-size: 0.9em;
}
</style>
