const { test, expect } = require('@playwright/test');

test.describe('Othello Game', () => {
  test.beforeEach(async ({ page }) => {
    await page.goto('/');
  });

  test('should display landing page with game mode options', async ({ page }) => {
    await expect(page.locator('h1.landing-title')).toHaveText('Othello');
    await expect(page.locator('text=Pass & Play')).toBeVisible();
    await expect(page.locator('text=Play as Black vs AI')).toBeVisible();
    await expect(page.locator('text=Play as White vs AI')).toBeVisible();
  });

  test('should start Pass & Play game', async ({ page }) => {
    await page.click('text=Pass & Play');
    
    // Check game board is visible
    await expect(page.locator('.game')).toBeVisible();
    
    // Check initial game state
    await expect(page.locator('text=Black\'s turn')).toBeVisible();
    await expect(page.locator('text=Black: 2 | White: 2')).toBeVisible();
    
    // Check back button is visible
    await expect(page.locator('text=← Back')).toBeVisible();
  });

  test('should allow making a move in Pass & Play mode', async ({ page }) => {
    await page.click('text=Pass & Play');
    
    // Wait for game to load
    await expect(page.locator('.game')).toBeVisible();
    
    // Click on a legal move
    const legalMove = page.locator('.box-shadow-with-hover-effect').first();
    await legalMove.click();
    
    // Wait for animation
    await page.waitForTimeout(1000);
    
    // Check turn changed
    await expect(page.locator('text=White\'s turn')).toBeVisible();
  });

  test('should start AI game as Black', async ({ page }) => {
    await page.click('text=Play as Black vs AI');
    
    await expect(page.locator('.game')).toBeVisible();
    await expect(page.locator('text=Your turn')).toBeVisible();
  });

  test('should make AI move after player move', async ({ page }) => {
    await page.click('text=Play as Black vs AI');
    
    // Make player move
    const legalMove = page.locator('.box-shadow-with-hover-effect').first();
    await legalMove.click();
    
    // Wait for player animation + AI thinking time + AI animation
    await page.waitForTimeout(4000);
    
    // AI should have made a move
    await expect(page.locator('text=Your turn')).toBeVisible();
  });

  test('should navigate back to landing page', async ({ page }) => {
    await page.click('text=Pass & Play');
    await page.click('text=← Back');
    
    await expect(page.locator('h1.landing-title')).toHaveText('Othello');
  });

  test('should start new game', async ({ page }) => {
    await page.click('text=Pass & Play');
    
    // Make a move
    const legalMove = page.locator('.box-shadow-with-hover-effect').first();
    await legalMove.click();
    await page.waitForTimeout(1000);
    
    // Click New Game
    await page.click('text=New Game');
    
    // Should reset to initial state
    await expect(page.locator('text=Black\'s turn')).toBeVisible();
    await expect(page.locator('text=Black: 2 | White: 2')).toBeVisible();
  });

  test('should highlight last move', async ({ page }) => {
    await page.click('text=Pass & Play');
    
    const legalMove = page.locator('.box-shadow-with-hover-effect').first();
    await legalMove.click();
    
    // Wait for animation
    await page.waitForTimeout(1000);
    
    // Check for highlight class
    await expect(page.locator('.highlight')).toBeVisible();
  });

  test('should disable clicking during AI turn', async ({ page }) => {
    await page.click('text=Play as White vs AI');
    
    // Wait for game to load - it's AI's turn first
    await expect(page.locator('text=AI is thinking...')).toBeVisible();
    
    // Should not have any clickable cells during AI turn
    const clickableCells = page.locator('.box-shadow-with-hover-effect');
    await expect(clickableCells).toHaveCount(0);
  });
});