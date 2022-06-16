module.exports = {
  content: ['./src/**/*'],
  theme: {
    extend: {
      animation: {
        roll: 'spin 0.5s linear'
      }
    },
  },
  plugins: [
    require('@tailwindcss/typography'),
    require('daisyui')
  ],
}
