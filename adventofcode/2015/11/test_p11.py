#!/usr/bin/python3

import unittest

import butts

class TestP08(unittest.TestCase):

    def test_has_straight(self):
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxabcyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxbcdyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxcdeyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxdefyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxefgyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxfghyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxtuvyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxuvwyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxvwxyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxwxyyy'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xxxyzyy'))
        self.assertFalse(butts.PasswordIncrementer.test_has_straight('abd'))
        self.assertTrue(butts.PasswordIncrementer.test_has_straight('xaoeuabc'))

    def test_has_two_doubles(self):
        self.assertTrue(butts.PasswordIncrementer.test_has_two_doubles('fisticuffspunchingcattle'))
        self.assertTrue(butts.PasswordIncrementer.test_has_two_doubles('aaxx'))
        self.assertFalse(butts.PasswordIncrementer.test_has_two_doubles('axxx'))

    def test_has_no_ugly_chars(self):
        self.assertFalse(butts.PasswordIncrementer.test_no_uggos('abcdefghijklmnopqrstuvwxyz'))
        self.assertTrue(butts.PasswordIncrementer.test_no_uggos('abcdefghjkmnpqrstuvwxyz'))

    def test_increment(self):
        pi = butts.PasswordIncrementer('abcdefgh')
        self.assertEqual(next(pi), 'abcdffaa')

        pi = butts.PasswordIncrementer('vzbxkghb')
        self.assertEqual(next(pi), 'vzbxxyzz')
        self.assertEqual(next(pi), 'lol')

        pi = butts.PasswordIncrementer('ghijklmn')
        self.assertEqual(next(pi), 'ghjaabcc')

if __name__ == '__main__':
    unittest.main()

