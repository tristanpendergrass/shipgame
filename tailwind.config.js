module.exports = {
  content: ['./src/**/*'],
  theme: {
    extend: {
      animation: {
        roll: 'spin 0.3s linear'
      },
      keyframes: {
        rotate: {
          '0%': {transform: 'rotate(0deg)'},
          '100%': {transform: 'rotate(360deg)'}
        }
      }

    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('daisyui')
  ],
}
