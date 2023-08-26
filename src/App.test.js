import { render, screen } from '@testing-library/react';
import App from './App';

test('renders the root component', () => {
  render(<App />);
  const linkElement = screen.getByText(/the root component/i);
  expect(linkElement).toBeInTheDocument();
});
