export function useGamePairLabels() {

const shapeSymbols: { [key: string]: string } = {
    triangle: '▲',
    rectangle: '▬',
    square: '■',
    hexagon: '⬢',
    circle: '⬤'
  }
  const getShapeList = (): string[] => {
    return Object.keys(shapeSymbols);
  }
  const getShapeSymbol = (shape: string): string => {
    return shapeSymbols[shape] || '?'
  }
  
  
  const colorValue: { [key: string]: string } = {
    red: 'red',
    green: 'green',
    blue: 'blue',
    yellow: 'yellow',
    black: 'black'
  }
  
  const getColorList = (): string[] => {
    return Object.keys(colorValue);
  }
  
  const getColorValue = (color: string): string => {
    return colorValue[color] || '?'
  }


    return {
        getShapeList:getShapeList,
        getShapeSymbol:getShapeSymbol,
        getColorList:getColorList,
        getColorValue:getColorValue,
     }
}