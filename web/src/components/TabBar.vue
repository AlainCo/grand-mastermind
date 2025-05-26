<script setup lang="ts">
import { ref } from 'vue'
import { useRouter,useRoute  } from 'vue-router' 

const router = useRouter()
const route = useRoute()
const activeTab = ref('home') 

const changeTab = (tab: string, mode: string = 'attack', stage: string = 'initialize') => {
  activeTab.value = tab
  if (tab === 'home') {
    router.push(`/home/${mode}/${stage}`)
  } else {
    router.push(`/${tab}`)
  }
}

const currentMode = ref(route.params.mode || 'attack')
const currentStage = ref(route.params.stage || 'initialize')

const toggleMode = () => {
  currentMode.value = currentMode.value === 'attack' ? 'defense' : 'attack'
  router.push(`/home/${currentMode.value}/${currentStage.value}`)
}

const toggleStage = () => {
  currentStage.value = currentStage.value === 'initialize' ? 'play' : 'initialize'
  router.push(`/home/${currentMode.value}/${currentStage.value}`)
}

</script>

<template>
  <div class="tab-bar">
    <button :class="{ active: activeTab === 'home' }" 
      @click="changeTab('home', currentMode as string, currentStage as string)">Home</button>
    <button :class="{ active: activeTab === 'about' }" 
      @click="changeTab('about')">About</button>
    <button @click="toggleMode">{{ currentMode === 'attack' ? 'Defense' : 'Attack' }}</button>
    <button @click="toggleStage">{{ currentStage === 'initialize' ? 'Play' : 'Initialize' }}</button>
  </div>
</template>

<style scoped>
.tab-bar {
  display: flex;
  justify-content: center;
  align-items: center;
  margin-top: 1rem;
}

.tab-bar button {
  padding: 0.5rem 1rem;
  border: 1px solid #ccc;
  border-radius: 4px;
  margin-right: 0.5rem; /* Adjust styling as needed */
}

.tab-bar button.active {
  background-color: #f0f0f0;
}
</style>