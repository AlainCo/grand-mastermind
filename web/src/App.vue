<script setup lang="ts">
import {  RouterView } from 'vue-router'
import TabBar from './components/TabBar.vue' 
</script>

<template>
  <div class="app-container">
    <TabBar />
    <RouterView />
  </div>
</template>

<style scoped>
.app-container {
  display: flex;
  flex-direction: column;
  height: 100vh;
}
</style>
