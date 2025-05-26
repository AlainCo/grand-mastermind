<script setup lang="ts">
import GameBoard  from "@/components/GameBoard.vue"
</script>


<template>
  <div class="home">
    <GameBoard />
  </div>
</template>


<style scoped>
.home {
  /*max-width: 800px;*/
  margin: 0 auto;
  padding: 20px;
}

.home h1 {
  text-align: center;
}

</style>
