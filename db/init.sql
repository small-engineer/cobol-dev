CREATE TABLE IF NOT EXISTS journal_entry (
    id              SERIAL PRIMARY KEY,
    entry_date      DATE NOT NULL,
    debit_account   VARCHAR(50) NOT NULL,
    credit_account  VARCHAR(50) NOT NULL,
    amount          INTEGER NOT NULL CHECK (amount >= 0),
    memo            TEXT
);

CREATE INDEX IF NOT EXISTS idx_journal_entry_date
    ON journal_entry (entry_date);

INSERT INTO journal_entry (entry_date, debit_account, credit_account, amount, memo)
VALUES 
  ('2025-05-01', '現金', '売上', 10000, '売上計上'),
  ('2025-05-02', '仕入', '現金', 5000, '仕入支払い');