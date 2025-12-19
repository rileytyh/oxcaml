import { test, expect } from '@playwright/test';

const URL = process.env.GAME_URL ?? 'http://127.0.0.1:8899/';

test('page renders', async ({ page }) => {
  await page.goto(URL);
  await expect(page.getByTestId('title')).toBeVisible();
  await expect(page.getByTestId('board-svg')).toBeVisible();
  await expect(page.getByTestId('btn-roll')).toBeVisible();
});

test('roll shows dice row', async ({ page }) => {
  await page.goto(URL);

  await expect(page.getByTestId('dice-row')).toHaveCount(0);

  await page.getByTestId('btn-roll').click();

  await expect(page.getByTestId('dice-row')).toBeVisible();
  await expect(page.getByTestId('die-0')).toBeVisible();
});

test('end turn switches player', async ({ page }) => {
  await page.goto(URL);

  await expect(page.getByTestId('status-bar')).toBeVisible();

  await page.getByTestId('btn-roll').click();
  await page.getByTestId('btn-end').click();

  await expect(page.getByTestId('status-meta')).toContainText('turn=black');
});

test('new game resets UI', async ({ page }) => {
  await page.goto(URL);

  await page.getByTestId('btn-roll').click();
  await expect(page.getByTestId('dice-row')).toBeVisible();

  await page.getByTestId('btn-new').click();

  await expect(page.getByTestId('dice-row')).toHaveCount(0);
  await expect(page.getByTestId('status-meta')).toContainText('turn=white');
});
