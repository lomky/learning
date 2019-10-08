require_relative '../../../app/api'

module ExpenseTracker
  RSpec.describe API do
    describe 'POST /expenses' do
      context 'when the expense is successfully recorded' do
        it 'requires the expense id'
        it 'response with a 200 (OK)'
      end

      context 'when it fails validation' do
        it 'returns an error message'
        it 'respomse woith a 422 (Unprocessable entity)'
      end

      # Next
    end
  end
end
